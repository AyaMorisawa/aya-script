module Main where

import System.Environment
import AyaScript (compile)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then do
      code <- getContents
      putStrLn $ either show id $ compile code
    else do
      let code = head args
      putStrLn $ either show id $ compile code
