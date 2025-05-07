module Interpreter (runProgram) where

import System.Directory (doesFileExist)
import Control.Exception (IOException, throwIO, handle)
import Data.List (intercalate, sort, isPrefixOf, isSuffixOf, isInfixOf)
import Control.Monad (forM_, foldM)
import Data.Char (isSpace)
import Parser
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Control.Exception (Exception, throwIO)

type Env = [(String, [[String]])]

data InterpreterError
  = VarAlreadyDefined String
  | MissingVariable String
  | FileNotFound String
  | ArityMismatch String
  | InvalidOperation String
  deriving (Show)

instance Exception InterpreterError

runProgram :: [Stmt] -> IO ()
runProgram stmts = handle printInterpreterError $ interpret [] stmts

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError err = putStrLn $ "Runtime error: " ++ show err


--Interprets each line updating enviroment each time
interpret :: Env -> [Stmt] -> IO ()
interpret env stmts = do
  _ <- foldM interpretStmt env stmts
  return ()





-- Handle a single statement
interpretStmt env (Assign outName op) = do
  case lookup outName env of
    Just _  -> throwIO $ VarAlreadyDefined outName
    Nothing -> do
      result <- evalOperation env op
      let env' = (outName, result) : env
      return env'

-- Standalone operation (without assignment)
interpretStmt env (StmtOp op) = interpretStmt env (Assign "out" op)

-- Outputs a file to cmd line
interpretStmt env (OutputStmt file) = do
  checkInEnv env "Output" file
  rows <- load env file
  forM_ (sort rows) (putStrLn . rowToCSV)
  return env











-- Evaluate any operation into resulting rows
evalOperation :: Env -> Operation -> IO [[String]]
evalOperation env (ProductOp f1 f2) = do
  checkInEnv env "product" f1
  checkInEnv env "product" f2
  a <- load env f1
  b <- load env f2
  return [ r1 ++ r2 | r1 <- a, r2 <- b ]

evalOperation env (LeftJoinOp f1 f2) = do
  checkInEnv env "leftJoin" f1
  checkInEnv env "leftJoin" f2
  joinWith env mergeRows f1 f2

evalOperation env (RightJoinOp f1 f2) = do
  checkInEnv env "rightJoin" f1
  checkInEnv env "rightJoin" f2
  joinWith env mergeRows f2 f1

evalOperation env (InnerJoinOp f1 f2) = do
  checkInEnv env "innerJoin" f1
  checkInEnv env "innerJoin" f2
  a <- load env f1
  b <- load env f2
  return [ mergeRows r1 r2 | r1 <- a, r2 <- b, safeIndex 0 r1 == safeIndex 0 r2 ]

evalOperation _ (ReadOp f) = do
  a <- readNew f
  return a

evalOperation env (AppendOp f1 f2) = do
  checkInEnv env "Append" f1
  checkInEnv env "Append" f2
  a <- load env f1
  b <- load env f2
  checkArity a f1 b f2
  return (a ++ b)

evalOperation env (DifferenceOp f1 f2) = do
  checkInEnv env "Difference" f1
  checkInEnv env "Difference" f2
  a <- load env f1
  b <- load env f2
  checkArity a f1 b f2
  let setA = Set.fromList a
      setB = Set.fromList b
      diff = Set.toList (Set.difference setA setB)
  return diff

evalOperation env (IntersectionOp f1 f2) = do
  checkInEnv env "Append" f1
  checkInEnv env "Append" f2
  a <- load env f1
  b <- load env f2
  checkArity a f1 b f2
  let setA = Set.fromList a
      setB = Set.fromList b
      common = Set.toList (Set.intersection setA setB)
  return common

evalOperation env (UnionOp f1 f2) = do
  checkInEnv env "Union" f1
  checkInEnv env "Union" f1
  a <- load env f1
  b <- load env f2
  checkArity a f1 b f2
  let combined = a ++ b
      uniqueRows = Set.toList (Set.fromList combined)
  return uniqueRows

evalOperation env (FilterOp f cond) = do
  checkInEnv env "filter" f
  rows <- load env f
  return (filter (evalCond cond) rows)

evalOperation env (SelectOp items files) = do
  case files of
    [f] -> do
      checkInEnv env "Select" f
      rows <- load env f
      return [ concatMap (resolve row) items | row <- rows ]
    _ -> throwIO $ InvalidOperation "Select only supports one input file"

evalOperation env (DistinctOp file col) = do
  checkInEnv env "Distinct" file
  rows <- load env file
  return $ distinctByCol col rows

evalOperation env (TopOp file n) = do
  checkInEnv env "Top" file
  rows <- load env file
  return $ take n rows

evalOperation env (BottomOp file n) = do
  checkInEnv env "Bottom" file
  rows <- load env file
  return $ reverse (take n (reverse rows))

-- Merge two rows using 'take from a unless empty, else from b'
mergeRows :: [String] -> [String] -> [String]
mergeRows a b = a ++ b

-- Evaluate condition expression (supports ==, !=, <, >, AND, OR)
evalCond :: CondExpr -> [String] -> Bool
evalCond (CondSimple c) row = evalBaseCond c row
evalCond (CondAnd a b) row = evalCond a row && evalCond b row
evalCond (CondOr a b) row = evalCond a row || evalCond b row

evalBaseCond :: Cond -> [String] -> Bool
evalBaseCond (Cond i1 comp i2) row =
  let v1 = resolveIdent i1 row  -- actual value from CSV row
      v2 = resolveIdent i2 row  -- pattern or string to match against
  in case comp of
    Eq         -> v1 == v2
    InEq       -> v1 /= v2
    Lt         -> v1 < v2
    Gt         -> v1 > v2
    StartsWith -> v2 `isPrefixOf` v1
    EndsWith   -> v2 `isSuffixOf` v1
    Contains   -> v2 `isInfixOf` v1









-- Turn Ident into string
resolveIdent :: Ident -> [String] -> String
resolveIdent (IdStr s) _   = s
resolveIdent (IdCol i) row = safeIndex i row
resolveIdent (IdFile f) _  = f  -- only meaningful if you're comparing file names literally


distinctByCol :: Int -> [[String]] -> [[String]]
distinctByCol col rows = go Set.empty rows
  where
    go _ [] = []
    go seen (r:rs)
      | val `Set.member` seen = go seen rs
      | otherwise = r : go (Set.insert val seen) rs
      where val = safeIndex col r



-- Convert SelectItem into string value
resolve :: [String] -> SelectItem -> [String]
resolve row (SelCol i)    = [safeIndex i row]
resolve _   (SelStr s)    = [s]
resolve row  SelAll       = row  -- return all fields
resolve row (SelCond (Conditional cond thenItem elseItem)) =
  if evalCond cond row
    then resolve row thenItem
    else resolve row elseItem











-- Utilities
checkInEnv :: Env -> String -> String -> IO ()
checkInEnv env opName name =
  case lookup name env of
    Just _  -> return ()
    Nothing -> throwIO $ MissingVariable $ opName ++ " — file not found: " ++ name

checkArity :: [[String]] -> String -> [[String]] -> String -> IO ()
checkArity f1 f1Name f2 f2Name =
  let f1A = if null f1 then 0 else length (head f1)
      f2A = if null f2 then 0 else length (head f2)
  in if f1A == f2A
       then return ()
       else throwIO $
         ArityMismatch $
           f1Name ++ " has " ++ show f1A ++ " columns, " ++
           f2Name ++ " has " ++ show f2A ++ " columns."


safeIndex :: Int -> [String] -> String
safeIndex i row = if i < length row then row !! i else ""

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

rowToCSV :: [String] -> String
rowToCSV = intercalate ","
 
--read a new file in from a csv file in memory   
readNew :: String -> IO [[String]]
readNew file = do
  let path = file ++ ".csv"
  exists <- doesFileExist path
  if not exists
    then throwIO $ FileNotFound $ "CSV file not found: " ++ path
    else do
      rows <- handle
        (\e -> throwIO $ FileNotFound $ "Error reading CSV file " ++ path ++ ": " ++ show (e :: IOException))
        (readCSV path)
      return $ map (map trim) rows



--load an existing file in the enviroment
load :: Env -> String -> IO [[String]]
load env name =
  case lookup name env of
    Just rows -> return (rows)
    Nothing -> throwIO $ MissingVariable $ "File not found in environment: " ++ name

joinWith :: Env -> ([String] -> [String] -> [String]) -> String -> String -> IO [[String]]
joinWith env merge f1 f2 = do
  a <- load env f1
  b <- load env f2
  return [ merge r1 r2 | r1 <- a, r2 <- b, safeIndex 0 r1 == safeIndex 0 r2 ]

readCSV :: FilePath -> IO [[String]]
readCSV filePath = do
    content <- readFile filePath
    let linesOfFile = lines content
    return $ map (splitOn ",") linesOfFile

