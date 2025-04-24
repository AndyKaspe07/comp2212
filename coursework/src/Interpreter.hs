module Interpreter (runProgram) where


import Data.List (intercalate, sort, isPrefixOf, isSuffixOf, isInfixOf)
import Control.Monad (forM_, foldM)
import Data.Char (isSpace)
import Parser
import Data.List.Split (splitOn)
import qualified Data.Set as Set

type Env = [(String, [[String]])]

-- Initial function to run interpreter
runProgram :: [Stmt] -> IO ()
runProgram stmts = interpret initialEnv stmts
  where initialEnv = []  


--Interprets each line updating enviroment each time
interpret :: Env -> [Stmt] -> IO ()
interpret env stmts = do
  _ <- foldM interpretStmt env stmts
  return ()


-- Handle a single statement
interpretStmt :: Env -> Stmt -> IO Env

-- Assignment: result = <operation>
interpretStmt env (Assign outName op) = do
  putStrLn $ "Evaluating operation: " ++ show op ++ " -> " ++ outName
  result <- evalOperation env op
  let env' = (outName, result) : env
  forM_ result (putStrLn . rowToCSV)
  return env'

-- Standalone operation (without assignment)
interpretStmt env (StmtOp op) = interpretStmt env (Assign "out" op)

--Outputs a file to cmd line
interpretStmt env (OutputStmt file) =
  case lookup file env of
    Just rows -> do
      putStrLn $ "Outputting: " ++ file
      forM_ (sort rows) (putStrLn . rowToCSV)
      return env
    Nothing -> fail $ "Variable not found in environment: " ++ file





-- Evaluate any operation into resulting rows
evalOperation :: Env -> Operation -> IO [[String]]
evalOperation env (ProductOp f1 f2) = do
  a <- load env f1
  b <- load env f2
  return [ r1 ++ r2 | r1 <- a, r2 <- b ]

evalOperation env (LeftJoinOp f1 f2) = joinWith env mergeRows f1 f2

evalOperation env (RightJoinOp f1 f2) = joinWith env mergeRows f2 f1

evalOperation env (InnerJoinOp f1 f2) = do
  a <- load env f1
  b <- load env f2
  return [ mergeRows r1 r2 | r1 <- a, r2 <- b, safeIndex 0 r1 == safeIndex 0 r2 ]

evalOperation _ (ReadOp f) = do
  a <- readNew f
  return a
  

evalOperation env (FilterOp f cond) = do
  rows <- load env f
  return (filter (evalCond cond) rows)

evalOperation env (SelectOp items files) = do
  case files of
    [f] -> do
      rows <- load env f
      return [ map (resolve row) items | row <- rows ]
    _ -> fail "Select only supports one input file"

evalOperation env (DistinctOp file col) = do
  rows <- load env file
  return $ distinctByCol col rows

evalOperation env (TopOp file n) = do
  rows <- load env file
  return $ take n rows

evalOperation env (BottomOp file n) = do
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
resolve :: [String] -> SelectItem -> String
resolve row (SelCol i)    = safeIndex i row
resolve _   (SelStr s)    = s
resolve _   SelAll        = error "* not supported here"
resolve row (SelCond (Conditional cond thenItem elseItem)) =
  if evalCond cond row
    then resolve row thenItem
    else resolve row elseItem



-- Utilities
safeIndex :: Int -> [String] -> String
safeIndex i row = if i < length row then row !! i else ""

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

rowToCSV :: [String] -> String
rowToCSV = intercalate ","
 
--read a new file in from a csv file in memory   
readNew :: String -> IO [[String]]
readNew file = do
  rows <- readCSV (file ++ ".csv")
  return $ map (map trim) rows

--load an existing file in the enviroment
load :: Env -> String -> IO [[String]]
load env name =
  case lookup name env of
    Just rows -> return rows
    Nothing   -> fail $ "File not found: " ++ name

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

