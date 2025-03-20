module Main (
    main
  ) where

import Lib (introMessage)
import System.Environment (getArgs)

main :: IO ()
main = do
    print introMessage