module Main where

import Data.List (isPrefixOf)
import qualified Data.Map as Map
import RunFile (interpFile)
import System.Environment (getArgs)

{- ARGUMENT PARSING -}
printUsage :: String -> IO ()
printUsage progName = putStrLn ("usage: " ++ progName ++ " <input file>")

splitOne :: Char -> String -> (String, String)
splitOne ch str =
  let helper :: String -> String -> (String, String)
      helper (x : xs) acc = if x == ch then (reverse acc, xs) else helper xs (x : acc)
      helper [] acc = (reverse acc, [])
   in helper str []

data CliOption = T | S String deriving (Show)

parseArgs :: [String] -> Map.Map String CliOption
parseArgs args = helper args []
 where
  helper :: [String] -> [(String, CliOption)] -> Map.Map String CliOption
  helper [] acc = Map.fromList acc
  helper (x : xs) acc = helper xs ((opt, arg) : acc)
   where
    (l, r) = splitOne '=' x
    opt = if "--" `isPrefixOf` l then l else "filename"
    arg
      | "--" `isPrefixOf` l =
          if null r
            then T
            else S r
      | null r = S l
      | otherwise = error "option must start with --"

main :: IO ()
main = do
  args <- getArgs
  case Map.lookup "filename" (parseArgs args) of
    Nothing -> putStrLn "no input file"
    Just T -> putStrLn "something went wrong in parse"
    Just (S filename) -> interpFile filename
