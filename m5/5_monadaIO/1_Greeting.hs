module Main where

import           Data.Char                      ( isSpace )
import           System.IO                      ( hFlush
                                                , stdout
                                                )

main :: IO ()
main = main'

main' :: IO ()
main' = do
  putStrLn "What is your name?"
  name <- getName
  putStrLn $ "Hi, " ++ name ++ "!"


getName :: IO String
getName = do
  putStr "Name: "
  hFlush stdout
  name <- getLine
  if all isSpace name then getName else return name
