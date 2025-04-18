import Lexer
import Parser
import Interpreter (interpret)
import System.Environment (getArgs)
import Control.Exception (catch, ErrorCall, displayException)
import System.IO


main :: IO ()
main = catch main' noParse

main' :: IO ()
main' = do
  (fileName : _) <- getArgs
  sourceText <- readFile fileName
  let parsedProg = parseProgram (alexScanTokens sourceText)
  interpret parsedProg

noParse :: ErrorCall -> IO ()
noParse e = hPutStrLn stderr ("Error: " ++ displayException e)