
{--
  CSCI 312 Homework #2

  Adpated from https://cs.pomona.edu/~michael/courses/csci131s18/hw/Hw02.html

Wonjun Jo
CSCI 312: Programming Language Design
Professor Simon Levy
Assignment #2
02/04/26

--}


module Hw02 where

data ArithExp =
    Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp

data ArithExp' =
    Num' Int
  | Plus' ArithExp' ArithExp'
  | Sub' ArithExp' ArithExp'
  | Times' ArithExp' ArithExp'
  | Neg' ArithExp'
  deriving Show

-- Put your code here -------------------------

-- Got some basic syntax for declaring show syntax from internet: "how to write show methods in haskell"
-- Also "how to use integers with strings in haskell"
instance Show ArithExp where
    show (Num n) = "Num " ++ show n
    show (Plus e1 e2) = "Plus (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (Times e1 e2) = "Times (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (Neg e1) = "Neg (" ++ show e1 ++ ")"

instance Eq ArithExp where
    e1 == e2 = show e1 == show e2

eval :: ArithExp -> Int
eval (Num n) = n
eval (Plus e1 e2) = eval e1 + eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Neg e1) = (-1) * eval e1


translate :: ArithExp' -> ArithExp
translate (Num' n) = Num n
translate (Plus' e1' e2') = Plus (translate e1') (translate e2')
translate (Sub' e1' e2') = Plus (translate e1') (Neg (translate e2'))
translate (Times' e1' e2') = Times (translate e1') (translate e2')
translate (Neg' e1') = Neg (translate e1')

eval' :: ArithExp' -> Int
eval' e1' = (eval . translate) e1'

instance Eq ArithExp' where
    e1' == e2' = eval' e1' == eval' e2'

-- simply got the syntax for https://stackoverflow.com/questions/3065954/defining-own-ord-for-a-data-type
instance Ord ArithExp' where
    compare e1' e2' = compare (eval' e1') (eval' e2')


data BST a = Empty | Node (BST a) a (BST a)

instance Show a => Show (BST a) where
    show Empty = ""
    show (Node x y z) = "( " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " )"

instance Functor BST where
    fmap f Empty = Empty
    fmap f (Node x y z) = Node (fmap f x) (f y) (fmap f z)


data RoseTree a = Leaf a | Branch [RoseTree a] deriving (Eq, Show)

-- used Claude https://claude.ai/chat/a56c8b7c-65e6-4d83-b599-48eccd4dd74d and tried for understanding afterwards
instance Functor RoseTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch r) = Branch (map (fmap f) r)


-- Tests: un-comment as you go ---------------

main = do

    putStrLn "Problem 1: arithmetic expressions -----------------------------------\n"

    putStr "\n(a) Should be Num 5: "
    print $ Num 5
    putStr "(a) Should be Neg (Plus (Num 1) (Num 1)): "
    print $ (Neg (Plus (Num 1) (Num 1)))
    putStr "\n(b) Should be True: " 
    print$ (Num 3) == (Num 3)
    putStr "(b) Should be False: " 
    print$ (Num 3) == (Num 4)
    putStr "(b) Should be True: " 
    print$ (Plus (Num 3) (Num 4)) == (Plus (Num 3) (Num 4))
    putStr "(b) Should be False: " 
    print $ (Plus (Num 3) (Num 4)) == (Num 7)

    putStr "\n(c) Should be 5: "

    print $ eval (Plus (Num 1) (Num 4))

    putStr "(c) Should be 0: "
    print $ eval (Plus (Num 42) (Neg (Num 42)))

    putStr "\n(d) Should be 2: "
    print $ eval' (Sub' (Num' 5) (Num' 3))

    putStr "(e) Should be False: " 
    print $ (Num' 2) == (Num' 3)
    putStr "(e) Should be True: " 
    print $ (Plus' (Num' 1) (Num' 2)) == (Num' 3)
    putStr "(e) Should be False: " 
    print $ (Num' 2) > (Num' 3)
    putStr "(e) Should be True: " 
    print $ (Plus' (Num' 1) (Num' 2)) < (Times' (Num' 2) (Num' 3))


    putStrLn "\nProblem 2: Functors ------------------------------------------------\n"
    putStr "\n(a) Should be ( (4) 6 (8) ): " 
    print $ fmap (\n -> 2 * n)(Node (Node Empty 2 Empty) 3 (Node Empty 4 Empty))

    putStr "\n(b) Should be Branch [Leaf 2,Leaf 3]: "
    print $ Branch [(Leaf 2), (Leaf 3)]

    putStr "\n(b) Should be: Branch [Leaf 1,Branch [Leaf 4,Leaf 9]]: "
    print $ fmap (\x -> x*x) (Branch [Leaf 1, (Branch [(Leaf 2), (Leaf 3)])])
    putStrLn ""

--}
