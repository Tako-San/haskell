module Main where

import           Data.List                      ( isInfixOf )

import           System.Directory               ( getDirectoryContents
                                                , removeFile
                                                )

import           System.IO                      ( hFlush
                                                , stdout
                                                )

main :: IO ()
main = main'

main' :: IO ()
main' = do
  putStr "Substring: "
  hFlush stdout
  subStr  <- getLine
  handleSubStr subStr

handleSubStr :: String -> IO ()
handleSubStr subStr = do
  case subStr of
    [] -> putStrLn "Cancelled"
    s -> rmAll s

rmAll :: String -> IO ()
rmAll subStr = do
  content <- getDirectoryContents "."
  let matched = filter (isInfixOf subStr) content
  mapM_ rmFile matched

rmFile :: String -> IO ()
rmFile file = do
  putStrLn $ "Removing file: " ++ file
  removeFile file
