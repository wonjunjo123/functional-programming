-- Adapted from https://cs.pomona.edu/~michael/courses/csci131s18/lec/Lec09.html

import Data.Char
import Control.Applicative

import qualified Data.Map as Map
import Data.Map (Map)

type VarName = String

type Store = Map.Map VarName Int

data AExp =
   Var String
 | Num Int
 | Plus AExp AExp
 | Times AExp AExp 
 | Neg AExp
 | Div AExp AExp
  deriving Show

data BExp a =
      Bool Bool -- Boolean atom
    | Equal a a
    | Lt a a
    | Not (BExp a)
    | Or (BExp a) (BExp a)
    | And (BExp a) (BExp a)
  deriving (Show, Eq)

data Stmt a b =
    Skip
  | Assign VarName a
  | Seq (Stmt a b) (Stmt a b)
  | If (b a) (Stmt a b) (Stmt a b)
  | While (b a) (Stmt a b)
  deriving (Show, Eq, Ord)

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

instance Functor Parser where
  fmap f p = Parser $ \s ->
    case parse p s of
      Nothing -> Nothing
      Just (v,s') -> Just (f v,s')

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a,s)
  f <*> a = Parser $ \s ->  -- f :: Parser (a -> b), a :: Parser a
    case parse f s of
      Nothing -> Nothing
      Just (g,s') -> parse (fmap g a) s' -- g :: a -> b, fmap g a :: Parser b

instance Alternative Parser where
  empty = Parser $ \s -> Nothing
  p1 <|> p2 = Parser $ \s ->
    case parse p1 s of
      Just (a,s') -> Just (a,s')
      Nothing -> parse p2 s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (x:xs) = if p x then Just (x,xs) else Nothing

spaces :: Parser ()
spaces = many (satisfy isSpace) *> pure ()

int :: Parser Int -- this really should be called a lexer
int = read <$> some (satisfy isDigit) -- read is a more abstract version of typecasting?

num :: Parser Int
num = spaces *> int

-- Help from gemini https://gemini.google.com/app/95298488f262a636
-- understood overall code, understood syntax, and motivation for defining boolean this way
boolean :: Parser Bool
boolean = ((const True) <$> ttrue)
         <|> ((const False) <$> tfalse)


aterm, afactor, aatom :: Parser AExp
aterm   =    Plus <$> afactor <* tplus <*> aterm
         <|> afactor
afactor =    Times <$> aatom <* ttimes <*> afactor 
         <|> aatom
aatom   =    Num <$> num 
         <|> Var <$> str
         <|> (tlpar *> aterm <* trpar)

aexp = aterm


-- got help from gemini for "not": https://gemini.google.com/app/95298488f262a636
-- also consulted claude further on "not': https://claude.ai/chat/9db0a778-8777-42bb-bb75-87f6456e8bc8
bterm, bfactor, batom :: Parser (BExp AExp)
bterm   =    Or <$> bfactor <* tor <*> bterm
         <|> bfactor
bfactor =    And <$> batom <* tand <*> bfactor 
         <|> batom
batom   =    Bool <$> boolean
         <|> Equal <$> aexp <* tequal <*> aexp
         <|> Lt <$> aexp <* tlt <*> aexp
         <|> Not <$> (Equal <$> aexp <* tnotequal <*> aexp)
         <|> (tlpar *> bterm <* trpar)

bexp = bterm

-- used claude to figure out if I need one overall function, or if I need sterm, sfactor, satom...
-- https://claude.ai/chat/7a2fb14b-c82d-4d82-9b64-0e977f3ee1ec
-- Then got more hints on while, but then was able to pattern match on my own for If
statement :: Parser (Stmt AExp BExp)
statement = Assign <$> str <* tassign <*> aexp
         <|> While <$> (twhile *> bexp) <* tdo <*> stmt <* tend
         <|> If <$> (tif *> bexp) <* tthen <*> stmt <* telse <*> stmt <* tend

stmt = statement

string :: Parser String                   
string = some (satisfy isAlpha)           

str :: Parser String                  
str = spaces *> string

keyword :: String -> (String, String)
keyword ('+':s)                 = ("+", s)
keyword ('*':s)                 = ("*", s)
keyword ('(':s)                 = ("(", s)
keyword (')':s)                 = (")", s)
keyword ('t':'r':'u':'e':s)     = ("true", s)
keyword ('f':'a':'l':'s':'e':s) = ("false", s)
keyword ('A':'N':'D':s)         = ("AND", s)
keyword ('O':'R':s)             = ("OR", s)
keyword ('=':s)                 = ("EQUAL", s)
keyword ('<':s)                 = ("<", s)
keyword ('!':'=':s)             = ("!=", s)
keyword (':':'=':s)             = (":=", s)
keyword ('W':'H':'I':'L':'E':s) = ("WHILE", s)
keyword ('D':'O':s)             = ("DO", s)
keyword ('E':'N':'D':s)         = ("END", s)
keyword ('I':'F':s)             = ("IF", s)
keyword ('T':'H':'E':'N':s)     = ("THEN", s)
keyword ('E':'L':'S':'E':s)     = ("ELSE", s)
keyword s = ([], s)

kw' :: String -> Parser String
kw' s =  Parser $ \s' ->
  let (value,rest) = keyword s' in
  if value == s then Just (s,rest)
  else Nothing

kw :: String -> Parser String
kw s = spaces *> (kw' s)

tlpar   = kw "("
trpar   = kw ")"
tplus   = kw "+"
ttimes  = kw "*"
ttrue = kw "true"
tfalse = kw "false"
tand = kw "AND"
tor = kw "OR"
tequal = kw "EQUAL"
tlt = kw "<"
tnotequal = kw "!="
tassign = kw ":="
twhile = kw "WHILE"
tdo = kw "DO"
tend = kw "END"
tif = kw "IF"
tthen = kw "THEN"
telse = kw "ELSE"

-- Tests -------------------------------------------

main :: IO()

main = do 

  putStrLn "\nPart 1: Test existing code -------------------------------------------------------"
  putStr "\nShould be Just (Plus (Num 3) (Num 4),\"\")\n          "
  print $ parse aexp "3 + 4"

  putStrLn "\nPart 2: Add support for variables ------------------------------------------------"
  putStr "\nShould be Just (Plus (Num 3) (Var \"x\"),\"\")\n          "
  print $ parse aexp "3 + x"

  putStrLn "\nPart 3: Add more keywords --------------------------------------------------------"
  putStr "\nShould be (\"true\",\" \")\n          "
  print $ keyword "true "
  putStr "\nShould be (\"false\",\" \")\n          "
  print $ keyword "false "
  putStr "\nShould be (\"AND\",\" \")\n          "
  print $ keyword "AND "
  putStr "\nShould be (\"OR\",\" \")\n          "
  print $ keyword "OR "
  putStr "\nShould be (\"\",\"bazinga!\")\n          "
  print $ keyword "bazinga!"

  putStrLn "\nPart 4: Add support for Boolean constants ----------------------------------------"
  putStr "\nShould be Just (True,\"\")\n          "
  print $ parse boolean "true"
  putStr "\nShould be Just (False,\"\")\n          "
  print $ parse boolean "false"
  putStr "\nShould be Nothing\n          "
  print $ parse boolean "bazinga!"
 
  putStrLn "\nPart 5: Add support for Boolean expressions --------------------------------------"
  putStr "\nShould be Just (Or (Bool False) (And (Bool True) (Bool True)),\"\")\n          "
  print $ parse bexp "false OR true AND true"
  putStr "\nShould be Just (And (Equal (Var \"x\") (Num 5)) (Lt (Var \"y\") (Num 7)),\"\")\n          "
  print $ parse bexp "x = 5 AND y < 7"
  putStr "\nShould be Just (Not (Equal (Num 3) (Num 5)),\"\")\n          "
  print $ parse bexp "3 != 5"

  putStrLn "\nPart 6: Add support for statements -----------------------------------------------"
  putStr "\nShould be Just (Assign \"x\" (Num 5),\"\")\n          "
  print $ parse stmt "x := 5"
  putStr "\nShould be Just (While (Bool True) (Assign \"x\" (Num 3)),\"\")\n          "
  print $ parse stmt "WHILE true DO x := 3 END"
  putStr "\nShould be Just (If (Not (Equal (Var \"x\") (Num 0))) "
  putStr "(Assign \"y\" (Num 10)) (Assign \"y\" (Var \"x\")),\"\")\n          "
  print $ parse stmt "IF x != 0 THEN y := 10 ELSE y := x END"

