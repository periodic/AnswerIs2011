{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Answer where

import Data.Maybe

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State

import Statistics.Math (factorial)

data Expression = Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Pow Expression Expression
                | Fact Expression
                | Sqrt Expression
                | Num
                deriving (Eq)

instance Show Expression where
    show e = fst $ runState (showST e) 1

showST :: Expression -> State Int String
showST (Add a b) = liftM2 (\a b -> "(" ++ a ++ "+" ++ b ++ ")") (showST a) (showST b)
showST (Sub a b) = liftM2 (\a b -> "(" ++ a ++ "-" ++ b ++ ")") (showST a) (showST b)
showST (Mul a b) = liftM2 (\a b -> "(" ++ a ++ "*" ++ b ++ ")") (showST a) (showST b)
showST (Div a b) = liftM2 (\a b -> "(" ++ a ++ "/" ++ b ++ ")") (showST a) (showST b)
showST (Pow a b) = liftM2 (\a b -> a ++ "**" ++ b) (showST a) (showST b)
showST (Fact e)  = liftM (++ "!") (showST e)
showST (Sqrt e)  = liftM (\s -> "sqrt(" ++ s ++ ")") (showST e)
showST (Num)  = do
    n <- get
    put (n+1)
    return . show $ n

newtype Eval a = E {
    runEval :: MaybeT (State Int) a
} deriving (Monad, MonadState Int)

evalST :: Expression -> Eval Double
evalST (Add a b) = liftM2 (+) (evalST a) (evalST b)
evalST (Sub a b) = liftM2 (-) (evalST a) (evalST b)
evalST (Mul a b) = liftM2 (*) (evalST a) (evalST b)
evalST (Div a b) = do
    denom <- evalST b
    if denom == 0
        then fail "Divide by zero."
        else liftM (/ denom) (evalST a)
evalST (Pow a b) = liftM2 (**) (evalST a) (evalST b)
evalST (Fact e)  = do
    base <- evalST e
    if base /= (fromIntegral . truncate $ base)
        then fail "Factorial of non-integer."
        else if base < 0 || base > 20
            then fail "Factorial too small or too large."
            else return . factorial . truncate $ base
evalST (Sqrt e)  = liftM sqrt (evalST e)
evalST (Num)  = do
    n <- get
    put (n+1)
    return . fromIntegral $ n


eval :: Expression -> Maybe Double
eval e = case evalState (runMaybeT . runEval . evalST $ e) 1 of
    Just a -> if (isNaN a) then Nothing else Just a
    a      -> a

enumerate :: Int -> Int -> [Expression]
enumerate unary binary = map (\(a, b, c) -> a) $ generate unary binary

generate :: Int -> Int -> [(Expression, Int, Int)]
generate u b = (Num, u, b) : facts u b ++ adds u b
    where
        facts u b | u == 0    = []
                  | otherwise = concatMap (mapUnary u b) [Fact, Sqrt]
        adds  u b | b == 0    = []
                  | otherwise = concatMap (mapBinary u b) [Add, Sub, Mul, Div, Pow]
        mapUnary  u b unary   = map (\(e, u, b) -> (unary e, u, b)) (generate (u - 1) b)
        mapBinary u b binary  = concatMap (\(e1, u1, b1) -> map (\(e2, u2, b2) -> (binary e1 e2, u2, b2)) (generate u1 b1)) (generate u (b-1))

