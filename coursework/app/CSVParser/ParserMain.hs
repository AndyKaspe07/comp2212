module Main (
    main ) where

import CSVParser (readCSV)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            putStrLn "CSV Content:"
            output <- readCSV file
            print output
        _ -> putStrLn "Usage: stack exec CSVParse-exe <csv-file>"