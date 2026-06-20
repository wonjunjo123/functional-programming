{--
  CSCI 312 Homework #1

  Adpated from https://cs.pomona.edu/~michael/courses/csci131s18/hw/Hw01.html

Wonjun Jo
CSCI 312: Programming Language Design
Professor Simon Levy
Assignment #1
1/26/26

--}

module Hw01 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List

type Node = String
type DAG = Map.Map Node (Set.Set Node)

a = "a"
b = "b"
c = "c"
d = "d"
e = "e"

g = Map.fromList [(a, Set.fromList [b,c]),
                  (b, Set.fromList [d]),
                  (c, Set.fromList [d]),
                  (d, Set.fromList []),
                  (e, Set.fromList [c])] -- modified to only have c from diagram on assignment

-- Put your functions here --------------------

-- Problem 1: Natural Recursion
sumUp :: [Int] -> Int
sumUp [] = 0
sumUp (x:xs) = x + sumUp(xs)

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) = if even x then [x] ++ evens(xs) else evens(xs) 

incAll :: [Int] -> [Int]
incAll [] = []
incAll (x:xs) = [x+1] ++ incAll(xs)

incBy :: Int -> [Int] -> [Int]
incBy n [] = []
incBy n (x:xs) = [x+n] ++ incBy n xs

-- struggled with it for roughly 20 minutes and then used
-- https://stackoverflow.com/questions/35442783/how-to-recursively-define-my-own-haskell-function-for-appending-two-lists
-- I made sure to reflect on why it is that I couldn't figure this out myself. Right now, Ijust think I need to get more familiar with thinking recursively and functionally

append :: [Int] -> [Int] -> [Int]
append [] ys = ys
append (x:xs) ys = x : append xs ys


-- Problem 2: Data Types
data IntTree = Empty | Node IntTree Int IntTree deriving (Eq,Show)

isLeaf :: IntTree -> Bool
isLeaf Empty = True
isLeaf (Node l x r) = (l == Empty) && (r == Empty)

sumTree :: IntTree -> Int
sumTree Empty = 0
sumTree (Node l x r) = sumTree l + x + sumTree r

fringe :: IntTree -> [Int]
fringe Empty = []
fringe (Node l x r) = if isLeaf (Node l x r) then [x] else fringe l ++ fringe r


-- Problem 3: Binary Search Trees

-- I first tried the method where I check l < x and x < r, but I ran into type error issues even after using Maybe (code at the end of this section)
-- So then I changed to checking if the list version of the tree is sorted
-- I wasn't a huge fan of this method because I had to write two helper functions
-- 
-- I used google, Claude, and youtube to get clarification on syntax and brainstorming
-- google search : "how to check if a list is sorted in haskell"
-- https://claude.ai/chat/0a9f1e6f-1be6-4612-bf90-ffa185089de8
-- https://www.youtube.com/watch?v=EjfoW-LXGbI

isBST :: IntTree -> Bool
isBST Empty = True
isBST (Node l x r) = isSorted (treeToList (Node l x r))

treeToList :: IntTree -> [Int]
treeToList Empty = []
treeToList (Node l x r) = (treeToList l) ++ [x] ++ (treeToList r)

isSorted :: [Int] -> Bool
isSorted xs = xs == sort xs

{--
isBST (Node l x r) = if (getValue l < x) && (x < getValue r) && (isBST l) && (isBST r) then True else False

getValue :: IntTree -> Maybe Int
getValue Empty = Nothing
getValue (Node _ value _) = Just value
--}

-- Problem 4: Map and Filter

-- I started with the same code as book and used Claude to better understand syntax and simplify implementation
sumUp' :: [Int] -> Int
sumUp' [] = 0
sumUp' l = foldl (+) 0 l

evens' :: [Int] -> [Int]
evens' [] = []
evens' l = filter even l

incAll' :: [Int] -> [Int]
incAll' [] = []
incAll' l = map (+1) l

incBy' :: Int -> [Int] -> [Int]
incBy' n [] = []
incBy' n l = map (+n) l


-- Problem 5: Defining Higher-order Functions

map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = [f x] ++ map1 f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f [] = []
filter1 f (x:xs) = if f x then [x] ++ filter1 f xs else filter1 f xs

-- Problem 6: Maybe and Either

--data Maybe Float = Nothing | Just Float

sqrt' :: Float -> Maybe Float
sqrt' x = if x < 0 then Nothing else Just (sqrt x)

div' :: Float -> Float -> Either String Float
div' a b = if b == 0 then Left "Division by 0 is undefined" else Right (a/b)


-- Problem 7: Creating Polymorphic Datatypes

-- I initially thought I had to create a variable tmp = Either a b...
swap :: (a,b) -> (b,a)
swap (a, b) = (b, a)

pairUp :: [a] -> [b] -> [(a,b)]
pairUp [] [] = []
pairUp (a:as) (b:bs) = [(a,b)] ++ pairUp as bs

splitUp :: [(a,b)] -> ([a], [b])
splitUp [] = ([], [])
splitUp ((a,b):xs) = ([a] ++ fst (splitUp xs), [b] ++ snd (splitUp xs))

sumAndLength :: [Int] -> (Int, Int)
sumAndLength [] = (0,0)
sumAndLength (x:xs) = (x + fst (sumAndLength xs), 1 + snd (sumAndLength xs))


-- Problem 8: Maps and Sets

-- This was a makeshift solution and doesn't address general cases (only handles nodes with up to two paths). My main issue was trying to extract the node from the neighbor set so that I could call the recursion.
-- https://claude.ai/chat/101e2f83-b10b-4839-a0f2-775f9ac5488d
hasPath :: DAG -> Node -> Node -> Bool
hasPath g "" "" = False
hasPath g "" n2 = False
hasPath g n1 "" = False
hasPath g n1 n2 = Set.member n2 (neighbors g n1)
    || hasPath g (fromMaybe "" (Set.lookupMin (neighbors g n1))) n2 -- call hasPath on first item
    || hasPath g (fromMaybe "" (Set.lookupMax (neighbors g n1))) n2 -- call hasPath on second item


-- For the neighbors function, I had a hard time trying to figure out how to parse the arguments correctly. I first tried things like neighbors (Node n (Set Node sn)) (Node a) = sn
-- but things weren't working out.
-- I initially used Claude to give me hints/nudges about how to approach this. However, I eventually asked for more direct help. In the future, I will work through this earlier and get help in office hours.
-- https://claude.ai/chat/62c16c8b-2292-4fcf-a5b0-2248011bedca
-- I had trouble dealing with the Maybe type, but now that I've learned how to do this, it makes a lot more intuitive sense now.

neighbors :: DAG -> Node -> Set.Set Node
neighbors g n = fromMaybe Set.empty (Map.lookup n g)

--extract :: Set.Set Node -> Int

--any' :: Set.Set Node -> Bool
--any' s = 


-- Tests ----------------------------------------


main = do

    putStrLn "Problem 1: natural recursion -----------------------------------\n"

    putStr "Should be 6: "
    print $ sumUp [1,2,3]

    putStr "Should be [2,4,6,8]: "
    print $ evens [1,2,3,4,5,6,7,8,9]

    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
    print $ incAll [1,2,3,4,5,6,7,8,9]

    putStr "Should be [3,4,5,6,7,8,9,10,11]: "
    print $ incBy 2 [1,2,3,4,5,6,7,8,9]

    putStr "Should be [1,2,3]: "
    print $ append [] [1,2,3]

    putStr "Should be [1,2,3]: "
    print $ append [1,2,3] []

    putStr "Should be [1,2,3,4,5,6]: "
    print $ append [1,2,3] [4,5,6]


    putStrLn "\nProblem 2: data types -----------------------------------------\n"

    putStr "Should be True: "
    print $ isLeaf Empty

    putStr "Should be True: "
    print $ isLeaf (Node Empty 3 Empty)

    putStr "Should be False: "
    print $ isLeaf (Node (Node Empty 1 Empty) 2 Empty)

    putStr "Should be 10: "
    print $ sumTree (Node (Node Empty 1 Empty) 3 (Node Empty 2 (Node Empty 4 Empty)))

    putStr "Should be [2,7]: "
    print $ fringe (Node (Node Empty 1 (Node Empty 2 Empty))
                          5
                          (Node (Node Empty 7 Empty) 10 Empty))

    putStrLn "\nProblem 3: binary search trees --------------------------------\n"

    putStr "Should be True: "
    print $ isBST (Node (Node Empty 2 Empty)  4 (Node Empty 5 Empty))

    putStr "Should be False: "
    print $ isBST (Node (Node Empty 5 Empty)  4 (Node Empty 2 Empty))

    putStrLn "\nProblem 4: map and filter -------------------------------------\n"

    putStr "Should be 6: "
    print $ sumUp' [1,2,3]

    putStr "Should be [2,4,6,8]: "
    print $ evens' [1,2,3,4,5,6,7,8,9]

    putStr "Should be [2,3,4,5,6,7,8,9,10]: "
    print $ incAll' [1,2,3,4,5,6,7,8,9]

    putStr "Should be [3,4,5,6,7,8,9,10,11]: "
    print $ incBy' 2 [1,2,3,4,5,6,7,8,9]


    putStrLn "\nProblem 5: defining higher-order functions --------------------\n"

    putStr "Should be [1,4,9,16,25]: "
    print $ map1 (\x -> x * x) [1,2,3,4,5]

    putStr "Should be [1,3,5,7,9]: "
    print $ filter1 odd [0,1,2,3,4,5,6,7,8,9]


    putStrLn "\nProblem 6: Maybe and Either ------------------------------------\n"

    putStr "Should be [0.0,1.0,2.0,3.0]: "
    print $ mapMaybe sqrt' [0,-1,1,-4,4,9,-9]


    putStrLn "\nProblem 7: Creating polymorphic data types ---------------------\n"

    putStr "Should be (\"hello\", 3): "
    print $ swap (3, "hello") 

    putStr "Should be [(0,1),(2,3),(4,5),(6,7),(8,9)]: "
    print $ pairUp [0,2,4,6,8] [1,3,5,7,9]

    putStr "Should be ([0,2,4,6,8],[1,3,5,7,9]): "
    print $ splitUp [(0,1),(2,3),(4,5),(6,7),(8,9)]

    putStr "Should be (15, 5): "
    print $ sumAndLength [1,2,3,4,5]

    case div' 1 0 of
      Right val -> print $ val
      Left  msg -> putStrLn msg

    case div' 1 2 of
      Right val -> print $ val
      Left  msg -> putStrLn msg


    putStrLn "\nProblem 8: maps and sets --------------------------------------\n"

    putStr "Should be True: "
    print $ hasPath g a d

    putStr "Should be False: "
    print $ hasPath g a e

    putStr "Although I pass the test cases, unfortunately, my implementation of hasPath does not solve for a general case. I wanted to bring this to your attention, as my code only handles cases with up to two paths per node. "
    putStr "Moving forward, I will work earlier and seek help during office hours."

