module Value where

import qualified Data.Array as Array
import GHC.Integer (divInteger)
import qualified LangUtils

data Value = Num Integer | Boolean Bool | StringLit (Array.Array Int Char) | Void deriving (Show)

-- prefix operators
valueNeg :: Value -> Value
valueNeg (Num a) = Num (-a)
valueNeg x = error ("-x not defined for x=" ++ show x)

valuePos :: Value -> Value
valuePos (Num a) = Num a
valuePos x = error ("+x not defined for x=" ++ show x)

valueNot :: Value -> Value
valueNot (Boolean b) = Boolean (not b)
valueNot x = error ("~x not defined for x=" ++ show x)

-- arithmetic operators
($+) :: Value -> Value -> Value
($+) (Num a) (Num b) = Num (a Prelude.+ b)
($+) (StringLit a) (StringLit b) = StringLit (LangUtils.concatArrays a b)
($+) l r = error ("+ operator not supported for " ++ show l ++ ", " ++ show r)

($-) :: Value -> Value -> Value
($-) (Num a) (Num b) = Num (a Prelude.- b)
($-) l r = error ("- operator not supported for " ++ show l ++ ", " ++ show r)

($*) :: Value -> Value -> Value
($*) (Num a) (Num b) = Num (a Prelude.* b)
($*) l r = error ("* operator not supported for " ++ show l ++ ", " ++ show r)

($/) :: Value -> Value -> Value
($/) (Num a) (Num b) = Num (divInteger a b)
($/) l r = error ("- operator not supported for " ++ show l ++ ", " ++ show r)

-- comparisson operators
($<) :: Value -> Value -> Value
($<) (Num a) (Num b) = Boolean (a Prelude.< b)
($<) l r = error ("< operator not supported for " ++ show l ++ ", " ++ show r)

($>) :: Value -> Value -> Value
($>) (Num a) (Num b) = Boolean (a Prelude.> b)
($>) l r = error ("> operator not supported for " ++ show l ++ ", " ++ show r)

($<=) :: Value -> Value -> Value
($<=) (Num a) (Num b) = Boolean (a Prelude.<= b)
($<=) l r = error ("<= operator not supported for " ++ show l ++ ", " ++ show r)

($>=) :: Value -> Value -> Value
($>=) (Num a) (Num b) = Boolean (a Prelude.>= b)
($>=) l r = error (">= operator not supported for " ++ show l ++ ", " ++ show r)

($==) :: Value -> Value -> Value
($==) (Num a) (Num b) = Boolean (a Prelude.== b)
($==) (Boolean a) (Boolean b) = Boolean (a Prelude.== b)
($==) (StringLit a) (StringLit b) = Boolean (LangUtils.arraysEqual a b)
($==) l r = error ("== operator not supported for " ++ show l ++ ", " ++ show r)

($~=) :: Value -> Value -> Value
($~=) l r = valueNot (l $== r)

-- logical operators
($&) :: Value -> Value -> Value
($&) (Boolean a) (Boolean b) = Boolean (a && b)
($&) l r = error ("& operator not supported for " ++ show l ++ ", " ++ show r)

($|) :: Value -> Value -> Value
($|) (Boolean a) (Boolean b) = Boolean (a || b)
($|) l r = error ("| operator not supported for " ++ show l ++ ", " ++ show r)