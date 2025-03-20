module CSVParser (
    readCSV ) where

import System.IO
import Data.List.Split (splitOn)

-- Function to read a CSV file and parse it into a list of lists
readCSV :: FilePath -> IO [[String]]
readCSV filePath = do
    content <- readFile filePath
    let linesOfFile = lines content
    return $ map (splitOn ",") linesOfFile

