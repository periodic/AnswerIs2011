{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Answer where

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Applicative
import Statistics.Math (factorial)

{- | The expression data structure.  Note that we don't specify the numbers because they are always the sequential natural numbers in the order they appear in the expression from left to right.
 -}
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

{- | Print out an expression.  Handles all the managing of state to make sure the numbers work out.
 -}
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

{- | The evaluation monad.  This needs to keep track of failure (such as divide
 - by zero or sqrt of a negative number) which is handled by MaybeT, and the
 - state to keep track of how many numbers have been seen so far so we can get
 - the correct number whenever we encounter one.
 -}
newtype Eval a = E {
    runEval :: MaybeT (State Int) a
} deriving (Monad, MonadState Int, MonadPlus, Functor, Applicative)

evalST :: Expression -> Eval Double
evalST (Add a b) = liftM2 (+) (evalST a) (evalST b)
evalST (Sub a b) = liftM2 (-) (evalST a) (evalST b)
evalST (Mul a b) = liftM2 (*) (evalST a) (evalST b)
evalST (Div a b) = liftM2 (/) (evalST a) (evalST b)
evalST (Pow a b) = liftM2 (**) (evalST a) (evalST b)
evalST (Fact e)  = do
    base <- evalST e
    guard (base == (fromIntegral . truncate $ base) && base > 0 && base < 20)
    return . factorial . truncate $ base
evalST (Sqrt e)  = liftM sqrt (evalST e)
evalST (Num)  = liftM fromIntegral $ get <* modify (+1)

{- | A wrapper to run the evaluation monad on an expression.
 -}
eval :: Expression -> Maybe Double
eval e = case evalState (runMaybeT . runEval . evalST $ e) 1 of
    Just a -> if isNaN a || isInfinite a
              then Nothing
              else Just a
    a      -> a

{- | The enumeration monad.  This is almost a state monad, but each element has
 - its own state.  This requires some slightly odd mechanics and limits our
 - ability to use newtype deriving.
 -}
newtype Enumerate st a = Gen {
        generate :: st -> [(a, st)]
    }

{- | When composing monads this works a lot like the list monad, but it has to
 - pass the state as well.  I'm not sure if this could be represented by some
 - combination of monad transformers.
 -}
instance Monad (Enumerate st) where
    return a = Gen $ \st -> [(a, st)]
    m >>= k  = Gen $ \st ->
                let vals = generate m st
                 in concatMap (\(v,st) -> generate (k v) st) vals
    fail _   = Gen $ \_ -> []

{- | In any specific instance we can get and put the state. -}
instance MonadState st (Enumerate st) where
    get    = Gen $ \st -> [(st, st)]
    put st = Gen $ \_  -> [((), st)]

{- | Monad plus instance for the use of msum and guard. -}
instance MonadPlus (Enumerate st) where
    mplus a b = Gen $ \st -> generate a st ++ generate b st
    mzero     = fail ""

{- | A short-hand for returning no values.
 -}
noVals :: Enumerate st a
noVals = fail ""

{- | The actual generator expression.  It tracks the number of unary and binary
 - expressions still available for use as it builds up an expression.
 -}
genExp :: Enumerate (Int, Int) Expression
genExp = msum $ num : map unary [Fact, Sqrt] ++ map binary [Add, Sub, Mul, Div, Pow]
    where
        num       = return Num
        unary cst = do
            (b, u) <- get
            guard (u > 0)
            put (b, u - 1)
            liftM cst genExp
        binary cst = do
            (b, u) <- get
            guard (b > 0)
            put (b - 1, u)
            liftM2 cst genExp genExp

{- | A wrapper for the generator.  Takes the maximum number of binary and unary
 - expressions to use.
 -}
enumerate :: Int -> Int -> [Expression]
enumerate binary unary = map fst . generate genExp $ (binary,unary)
