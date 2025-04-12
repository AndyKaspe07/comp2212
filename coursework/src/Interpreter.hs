module Interpreter (interpret) where

import CSVParser (readCSV)
import Data.List (intercalate, sort)
import Control.Monad (forM_)
import Data.Char (isSpace)
import System.IO
import Parser

-- Top-level interpreter: runs each statement
interpret :: [Stmt] -> IO ()
interpret = mapM_ interpretStmt

-- Handle a single statement
interpretStmt :: Stmt -> IO ()

-- Assignment: result = <operation>
interpretStmt (Assign outName op) = do
  putStrLn $ "Evaluating operation: " ++ show op ++ " → " ++ outName
  result <- evalOperation op
  forM_ (sort result) (putStrLn . rowToCSV)

-- Standalone operation (without assignment)
interpretStmt (StmtOp op) = interpretStmt (Assign "out" op)

-- Output a file directly
interpretStmt (OutputStmt file) = do
  putStrLn $ "Outputting file: " ++ file ++ ".csv"
  csv <- readCSV (file ++ ".csv")
  let clean = map (map trim) csv
  forM_ (sort clean) (putStrLn . rowToCSV)

-- Evaluate any operation into resulting rows
evalOperation :: Operation -> IO [[String]]
evalOperation (ProductOp f1 f2) = do
  a <- load f1
  b <- load f2
  return [ r1 ++ r2 | r1 <- a, r2 <- b ]

evalOperation (LeftJoinOp f1 f2) = joinWith mergeRows f1 f2

evalOperation (RightJoinOp f1 f2) = joinWith mergeRows f2 f1

evalOperation (InnerJoinOp f1 f2) = do
  a <- load f1
  b <- load f2
  return [ mergeRows r1 r2 | r1 <- a, r2 <- b, safeIndex 0 r1 == safeIndex 0 r2 ]

evalOperation (FilterOp f cond) = do
  rows <- load f
  return (filter (evalCond cond) rows)

evalOperation (SelectOp items files) = do
  case files of
    [f] -> do
      rows <- load f
      return [ map (resolve row) items | row <- rows ]
    _ -> fail "Select only supports one input file"

-- Merge two rows using 'take from a unless empty, else from b'
mergeRows :: [String] -> [String] -> [String]
mergeRows a b = zipWith merge (pad a) (pad b)
  where
    merge x y = if trim x == "" then y else x
    pad r = take 4 (r ++ repeat "")

-- Evaluate condition expression (supports ==, !=, <, >, AND, OR)
evalCond :: CondExpr -> [String] -> Bool
evalCond (CondSimple c) row = evalBaseCond c row
evalCond (CondAnd a b) row = evalCond a row && evalCond b row
evalCond (CondOr a b) row = evalCond a row || evalCond b row

-- Evaluate single condition
evalBaseCond :: Cond -> [String] -> Bool
evalBaseCond (Cond i1 comp i2) row =
  let v1 = resolveIdent i1 row
      v2 = resolveIdent i2 row
  in case comp of
    Eq   -> v1 == v2
    InEq -> v1 /= v2
    Lt   -> v1 < v2
    Gt   -> v1 > v2

-- Turn Ident into string
resolveIdent :: Ident -> [String] -> String
resolveIdent (IdStr s) _   = s
resolveIdent (IdCol i) row = safeIndex i row
resolveIdent (IdFile f) _  = f  -- only meaningful if you're comparing file names literally

-- Convert SelectItem into string value
resolve :: [String] -> SelectItem -> String
resolve row (SelCol i)    = safeIndex i row
resolve _   (SelStr s)    = s
resolve _   SelAll        = error "* not supported here"
resolve _   (SelCond _)   = error "IF-THEN-ELSE not implemented"

-- Utilities
safeIndex :: Int -> [String] -> String
safeIndex i row = if i < length row then row !! i else ""

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

rowToCSV :: [String] -> String
rowToCSV = intercalate ","

load :: String -> IO [[String]]
load file = do
  rows <- readCSV (file ++ ".csv")
  return $ map (map trim) rows

joinWith :: ([String] -> [String] -> [String]) -> String -> String -> IO [[String]]
joinWith merge f1 f2 = do
  a <- load f1
  b <- load f2
  return [ merge r1 r2 | r1 <- a, r2 <- b, safeIndex 0 r1 == safeIndex 0 r2 ]

