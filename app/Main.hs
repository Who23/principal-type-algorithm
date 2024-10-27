module Main where

import Data.Map (Map)

data LambdaCalculus = Var Char | Abs Char LambdaCalculus | App LambdaCalculus LambdaCalculus
data Type = Phi Int | Arrow Type Type

newtype Substitution = Substitution (Map Int Type)

instance Show LambdaCalculus where
    show (Var c) = [c]
    show (Abs a b) = "(λ" ++ [a] ++ "." ++ show b ++ ")"
    show (App m n) = "(" ++ show m ++ show n ++ ")"

instance Show Type where
    show (Phi i) = show i
    show (Arrow a b) = "(" ++ show a ++ ") -> (" ++ show b ++ ")"

unify :: Type -> Type -> Substitution
unify = undefined

pp :: LambdaCalculus -> Type
pp = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
