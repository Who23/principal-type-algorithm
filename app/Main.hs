module Main where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State

data LambdaCalculus = Var Char | Abs Char LambdaCalculus | App LambdaCalculus LambdaCalculus
data Type = Phi Int | Arrow Type Type

newtype Substitution = Substitution (Map Int Type) deriving Show

instance Show LambdaCalculus where
    show (Var c) = [c]
    show (Abs a b) = "(λ" ++ [a] ++ "." ++ show b ++ ")"
    show (App m n) = "(" ++ show m ++ show n ++ ")"

instance Show Type where
    show (Phi i) = show i
    show (Arrow a b) = "(" ++ show a ++ ") -> (" ++ show b ++ ")"

substitute :: Substitution -> Type -> Type
substitute (Substitution s) (Phi i) 
    | M.member i s   = s M.! i
    | otherwise      = Phi i
substitute s (Arrow a b) = Arrow (substitute s a) (substitute s b)

substituteCtx :: Substitution -> Map Char Type -> Map Char Type
substituteCtx s = M.map (substitute s)

composeSubstitution :: Substitution -> Substitution -> Substitution
composeSubstitution s1@(Substitution s1m) (Substitution s2m) = Substitution (M.union s1m s2m')
    where s2m' = M.map (substitute s1) s2m

unify :: Type -> Type -> Substitution
unify (Phi x) (Phi y)   = Substitution (M.singleton x (Phi y))
unify (Phi x) t         = Substitution (M.singleton x t)
unify t       p@(Phi _) = unify p t
unify (Arrow a b) (Arrow c d) = composeSubstitution s2 s1
    where
        s1 = unify a c
        s2 = unify (substitute s1 b) (substitute s1 d)

unifyCtx :: Map Char Type -> Map Char Type -> Substitution
unifyCtx gamma1 gamma2 = M.foldl composeSubstitution empty gamma'
    where
        gamma' = M.intersectionWith unify gamma1 gamma2
        empty = Substitution M.empty

pp :: LambdaCalculus -> Type
pp l = snd (evalState (pp' l) 0)
    where
        fresh :: State Int Int
        fresh = do i <- get
                   put (i + 1)
                   return i

        pp' :: LambdaCalculus -> State Int (Map Char Type, Type)
        pp' (Var x) = do phi <- fresh
                         return (M.singleton x (Phi phi), Phi phi)
        pp' (Abs x m) = do (gamma, p) <- pp' m
                           if M.member x gamma then
                                do let a = fromJust (M.lookup x gamma)
                                   let gamma' = M.delete x gamma
                                   return (gamma', Arrow a p)
                           else
                                do phi <- fresh
                                   return (gamma, Arrow (Phi phi) p)
        pp' (App m n) = do (gamma1, p1) <- pp' m
                           (gamma2, p2) <- pp' n
                           phi <- fresh
                           let s1 = unify p1 (Arrow p2 (Phi phi))
                           let s2 = unifyCtx (substituteCtx s1 gamma1) (substituteCtx s1 gamma2)
                           let s = composeSubstitution s2 s1 
                           let gamma1' = substituteCtx s gamma1
                           let gamma2' = substituteCtx s gamma2
                           let gamma = M.union gamma1' gamma2'
                           return (gamma, substitute s (Phi phi))
                                

sCombinator = Abs 'x' (Abs 'y' (Abs 'z' (App (App (Var 'x') (Var 'z')) (App (Var 'y') (Var 'z')))))
kCombinator = Abs 'x' (Abs 'y' (Var 'x'))
iCombinator = Abs 'x' (Var 'x')

main :: IO ()
main = putStrLn "Hello, Haskell!"
