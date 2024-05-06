module LangUtils where

import qualified Data.Set as Set

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates lst = length lst /= length (Set.fromList lst)
