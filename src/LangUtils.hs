module LangUtils where

import qualified Data.Array as Array
import qualified Data.Set as Set

type ArrayIC = Array.Array Int Char

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates lst = length lst /= length (Set.fromList lst)

stringToArray :: String -> ArrayIC
stringToArray str = Array.listArray (1, length str) str

concatArrays :: ArrayIC -> ArrayIC -> ArrayIC
concatArrays arr1 arr2 = Array.listArray (1, length arr1 + length arr2) (Array.elems arr1 ++ Array.elems arr2)

arraysEqual :: ArrayIC -> ArrayIC -> Bool
arraysEqual arr1 arr2 = all (uncurry (==)) (zip (Array.elems arr1) (Array.elems arr2))
