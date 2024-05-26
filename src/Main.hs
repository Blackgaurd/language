module Main where

import Cli (interpFile, minimizeFile)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Maybe
import System.Environment (getArgs)

{- ARGUMENT PARSING -}
printUsage :: String -> IO ()
printUsage progName = putStrLn ("usage: " ++ progName ++ " <input file>")

splitOne :: Char -> String -> (String, String)
splitOne ch str =
  let helper :: String -> String -> (String, String)
      helper (x : xs) acc =
        if x == ch
          then (reverse acc, xs)
          else helper xs (x : acc)
      helper [] acc = (reverse acc, [])
   in helper str []

data CliOption = T | F | S String deriving (Show, Eq)

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
          if null r then T else S r
      | null r = S l
      | otherwise = error "option must start with --"

argDefined :: Map.Map String CliOption -> String -> CliOption
argDefined args option =
  Data.Maybe.fromMaybe F (Map.lookup option args)

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
      filename = argDefined parsed "filename"
   in case filename of
        T -> putStrLn "something went wrong in parsing arguments"
        F -> putStrLn "no input file provided"
        S file ->
          if argDefined parsed "--minimize" == T
            then minimizeFile file >>= \out -> putStrLn out
            else interpFile file
