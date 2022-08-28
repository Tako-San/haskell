{-# LANGUAGE OverloadedStrings #-}

module ParsePerson where

import           Data.Text                      ( split
                                                , splitOn
                                                , unpack
                                                )

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving(Show, Read)

data Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  }
  deriving (Show, Read)

parsePerson :: String -> Either Error Person
parsePerson = undefined

toTuple3 :: [c] -> Maybe (c, c, c)
toTuple3 [a, b, c] = Just (a, b, c)
toTuple3 _         = Nothing

parseLines x = undefined

f str = maybe IncompleteDataError
              parseLines
              (toTuple3 . map unpack . splitOn "\n" $ str)

