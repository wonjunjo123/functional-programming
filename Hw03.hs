

{--
  CSCI 312 Homework #3

  Adpated from https://cs.pomona.edu/~michael/courses/csci131s18/hw/Hw03.html
--}

{-# OPTIONS_GHC -W #-}

module Hw03 where

import qualified Data.Map as Map
import Data.Maybe

-- Used claude to figure out that I want to use newtype instead of just type synonym
-- I might have had wrong approach here and did more than I need to...
-- https://claude.ai/chat/a4903333-c71c-4498-86a7-9a85c0b9f097
newtype Store = Store (Map.Map VarName Int)

-- Had to implement my own show method to unwrap the Store ( ) constructor
instance Show Store where
    show (Store s) = show s

type VarName = String

data ArithExp =
    Var VarName
  | Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp
  deriving (Show, Eq, Ord)

data BoolExp a =
    Bool Bool
  | Equal a a
  | Lt a a
  | Not (BoolExp a)
  | Or (BoolExp a) (BoolExp a)
  | And (BoolExp a) (BoolExp a)
  deriving (Show, Eq, Ord)

data Stmt a b =
    Skip
  | Assign VarName a
  | Seq (Stmt a b) (Stmt a b)
  | If (b a) (Stmt a b) (Stmt a b)
  | While (b a) (Stmt a b)
  deriving (Show, Eq, Ord)

-- Put your code here -------------------------

-- Basically, I was having trouble with Store not being in scope...
-- this led to other work arounds which I used Claude to help me troubleshoot
-- https://claude.ai/chat/a4903333-c71c-4498-86a7-9a85c0b9f097

evalA :: Store -> ArithExp -> Int
evalA (Store s) (Var x) = fromMaybe 0 (Map.lookup x s)
evalA _ (Num n) = n
evalA s (Plus x y) = (evalA s x) + (evalA s y)
evalA s (Times x y) = (evalA s x) * (evalA s y)
evalA s (Neg x) = (-1) * (evalA s x)


evalB :: Store -> BoolExp ArithExp -> Bool
evalB _ (Bool b) = b
evalB s (Equal x y) = (evalA s x) == (evalA s y)
evalB s (Lt x y) = (evalA s x) < (evalA s y)
evalB s (Not b) = not (evalB s b)
evalB s (Or b c) = (evalB s b) || (evalB s c)
evalB s (And b c) = (evalB s b) && (evalB s c)

eval :: Store -> Stmt ArithExp BoolExp -> Store
eval s Skip = s
eval (Store s) (Assign x y) = Store (Map.insert x (evalA (Store (s)) y) s)
eval s (Seq st1 st2) = eval (eval s st1) st2
eval s (If b st1 st2) = if (evalB s b) then (eval s st1) else (eval s st2)
eval s w@(While b st1) = if (evalB s b) then eval (eval s st1) w else s
-- Used claude to figure out if there was a way to reference both the entire wrapped variable and the unwrapped variable.

-- Tests ----------------------------------------

main = do

    putStrLn "Problem 1: Interpreting a language with variable assignment -----------------------\n"

    let store = Store (Map.fromList[("a", 2), ("b", 3)])
    
    print $ store

    putStr "(a) Should be -8: "
    print $ evalA store (Neg (Times (Var "a") (Plus (Num 1) (Var "b"))))

    putStr "\n(b) Should be True: "
    print $ evalB store (And (Lt (Var "a") (Var "b")) (Lt (Num 1) (Num 2)))
    putStr "(b) Should be False: "
    print $ evalB store (And (Lt (Var "b") (Var "b")) (Lt (Num 1) (Num 2)))
    putStrLn "\n"

    putStr "(c): Should be: fromList [(\"a\",2),(\"b\",3)]:  "
    print $ eval store Skip

    putStr "(c): Should be: fromList [(\"a\",4),(\"b\",3)]:  "
    print $ eval store (Assign "a" (Num 4))

    putStr "(c): Should be: fromList [(\"a\",5),(\"b\",3)]:  "
    print $ eval store (Seq (Assign "a" (Num 4)) (Assign "a" (Num 5)))

    putStr "(c): Should be: fromList [(\"a\",2),(\"b\",3),(\"c\",3)]:  "
    print $ eval store (If (Lt (Num 3) (Num 4)) (Assign "c" (Num 3)) Skip)

    putStr "(c): Should be: fromList [(\"a\",2),(\"b\",10)]: "
    print $ eval store (While (Lt  (Var "b") (Num 10)) (Assign "b" (Plus (Var "b") (Num 1))))





