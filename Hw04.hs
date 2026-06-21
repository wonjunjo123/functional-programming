{--
 - I must admit that I relied on Claude more than I would have liked to.
 - The initial process of understanding the recursions and the higher order functions of haskell were arduous
 - However, with each piece of code I used Claude, I spent much time understanding each syntax line by line and thinking about how I would have came to the same conclusion.
 - I feel that I understand parsing on a deeper level now and have appreciation for the applicative functors that we will use in the next assignment
 -
--}


module Hw04 where

import Control.Applicative
import Data.Char

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

type VarName = String

--type Store = Map.Map VarName Int
-- This newtype is an artifact from Hw03
newtype Store = Store (Map.Map VarName Int)

instance Show Store where
  show (Store s) = show s

data AExp =
    Var VarName
  | Num Int
  | Plus AExp AExp
  | Times AExp AExp
  | Neg AExp
  | Div AExp AExp
  deriving (Show, Eq)

data BExp a =
    Bool Bool
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

data Token =
    TNum Int
  | TId String
  | TPlus
  | TMinus
  | TTimes
  | TDiv
  | TLParen
  | TRParen
-- Add your tokens here ==============================
  | TFalse
  | TTrue
  | TOr
  | TAnd
  | TNot
  | TEqual
  | TSkip
  | TAssign
  | TIf
  | TUnequal
  | TThen
  | TElse
  | TEnd
  | TWhile
  | TDo
-- ===================================================
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (w:s) | isSpace w = lexer (dropWhile isSpace s)
lexer ('+':s) = TPlus:lexer s
lexer ('-':s) = TMinus:lexer s
lexer ('*':s) = TTimes:lexer s
lexer ('/':s) = TDiv:lexer s
lexer ('(':s) = TLParen:lexer s
lexer (')':s) = TRParen:lexer s
-- Add your lexer code here ==========================
lexer ('f':'a':'l':'s':'e':s) = TFalse:lexer s
lexer ('t':'r':'u':'e':s) = TTrue:lexer s
lexer ('O':'R':s) = TOr:lexer s
lexer ('A':'N':'D':s) = TAnd:lexer s
lexer ('N':'O':'T':s) = TNot:lexer s
lexer ('=':s) = TEqual:lexer s
lexer ('S':'K':'I':'P':s) = TSkip:lexer s
lexer (':':'=':s) = TAssign:lexer s
lexer ('I':'F':s) = TIf:lexer s
lexer ('!':'=':s) = TUnequal:lexer s
lexer ('T':'H':'E':'N':s) = TThen:lexer s
lexer ('E':'L':'S':'E':s) = TElse:lexer s
lexer ('E':'N':'D':s) = TEnd:lexer s
lexer ('W':'H':'I':'L':'E':s) = TWhile:lexer s
lexer ('D':'O':s) = TDo:lexer s

-- ===================================================

lexer s | isAlpha (head s) =
  let (id,s') = span isAlphaNum s in
  TId id:lexer s'
lexer s | isDigit (head s) =
  let (n,s') = span isDigit s in
  TNum (read n :: Int):lexer s'
lexer (n:_) = error $ "Lexer error: unexpected character " ++ [n]


parseATerm :: [Token] -> Either String (AExp,[Token])
parseATerm ts = 
  case parseAFactor ts of
    Right (f,ts) -> parseATerm' f ts
    Left e -> Left e
      
parseATerm' :: AExp -> [Token] -> Either String (AExp,[Token])
parseATerm' lhs [] = Right (lhs, [])
parseATerm' lhs (TPlus:ts) = parseATerm'' (Plus lhs) ts
parseATerm' lhs (TMinus:ts) = parseATerm'' (Plus lhs . Neg) ts
parseATerm' lhs ts = Right (lhs,ts)

parseATerm'' :: (AExp -> AExp) -> [Token] -> Either String (AExp,[Token])
parseATerm'' mk [] = Left $ "expected term after +/-"
parseATerm'' mk ts = 
  case parseAFactor ts of
    Right (e,ts) -> parseATerm' (mk e) ts
    Left e -> Left e

parseAFactor :: [Token] -> Either String (AExp,[Token])
parseAFactor ts =
  case parseANeg ts of
    Right (lhs,ts) -> parseAFactor' lhs ts
    Left e -> Left e
    
parseAFactor' :: AExp -> [Token] -> Either String (AExp,[Token])
parseAFactor' lhs (TTimes:ts) = parseAFactor'' (Times lhs) ts
parseAFactor' lhs (TDiv:ts) = parseAFactor'' (Div lhs) ts
parseAFactor' lhs ts = Right (lhs, ts)

parseAFactor'' :: (AExp -> AExp) -> [Token] -> Either String (AExp,[Token])
parseAFactor'' mk [] = Left $ "expected term after *"
parseAFactor'' mk ts = 
  case parseANeg ts of
    Right (e,ts) -> parseAFactor' (mk e) ts
    Left e -> Left e

parseANeg :: [Token] -> Either String (AExp, [Token])
parseANeg (TMinus:ts) = 
  case parseAAtom ts of
    Right (e,ts') -> Right (Neg e, ts')
    Left e -> Left e
parseANeg ts = parseAAtom ts

parseAAtom :: [Token] -> Either String (AExp, [Token])
parseAAtom (TNum n:ts) = Right (Num n, ts)
parseAAtom (TId id:ts) = Right (Var id, ts)
parseAAtom (TLParen:ts) =
  case parseATerm ts of
    Right (e,TRParen:ts') -> Right (e,ts')
    Right (_,ts) -> Left $ "expected right paren; found: " ++ show ts
    Left e -> Left e
parseAAtom ts = Left $ "expected number, identifier, or parens; found: " ++ show ts

tryParseATerm :: String -> AExp
tryParseATerm = tryParse parseATerm

tryParse :: ([Token] -> Either String (a,[Token])) -> String -> a
tryParse parser s =
  case parser $ lexer s of
    Right (e,[]) -> e
    Right (_,ts) -> error $ "Parse error: expected EOF; found: " ++ show ts
    Left e -> error $ "Parser error: " ++ e

-- Add your functions here ===============================================

parseBTerm :: [Token] -> Either String (BExp AExp,[Token])
parseBTerm ts = 
  case parseBFactor ts of
    Right (f,ts) -> parseBTerm' f ts
    Left e -> Left e

parseBTerm' :: BExp AExp -> [Token] -> Either String (BExp AExp,[Token])
parseBTerm' lhs [] = Right (lhs, [])
parseBTerm' lhs (TOr:ts) = parseBTerm'' (Or lhs) ts
parseBTerm' lhs ts = Right (lhs,ts)

parseBTerm'' :: (BExp AExp -> BExp AExp) -> [Token] -> Either String (BExp AExp,[Token])
parseBTerm'' mk [] = Left $ "expected term after +/-"
parseBTerm'' mk ts = 
  case parseBFactor ts of
    Right (e,ts) -> parseBTerm' (mk e) ts
    Left e -> Left e

parseBFactor :: [Token] -> Either String (BExp AExp,[Token])
parseBFactor ts =
  case parseBNeg ts of
    Right (lhs,ts) -> parseBFactor' lhs ts
    Left e -> Left e
    
parseBFactor' :: BExp AExp -> [Token] -> Either String (BExp AExp,[Token])
parseBFactor' lhs (TAnd:ts) = parseBFactor'' (And lhs) ts
parseBFactor' lhs ts = Right (lhs, ts)

parseBFactor'' :: (BExp AExp -> BExp AExp) -> [Token] -> Either String (BExp AExp,[Token])
parseBFactor'' mk [] = Left $ "expected term after *"
parseBFactor'' mk ts = 
  case parseBNeg ts of
    Right (e,ts) -> parseBFactor' (mk e) ts
    Left e -> Left e

parseBNeg :: [Token] -> Either String (BExp AExp, [Token])
parseBNeg (TNot:ts) = 
  case parseBAtom ts of
    Right (e,ts') -> Right (Not e, ts')
    Left e -> Left e
parseBNeg ts = parseBAtom ts

parseBAtom :: [Token] -> Either String (BExp AExp, [Token])
parseBAtom (TTrue:ts) = Right (Bool True, ts)
parseBAtom (TFalse:ts) = Right (Bool False, ts)
parseBAtom (TLParen:ts) =
  case parseBTerm ts of
    Right (e,TRParen:ts') -> Right (e,ts')
    Right (_,ts) -> Left $ "expected right paren; found: " ++ show ts
    Left e -> Left e
-- Used claude for Equal (https://claude.ai/chat/27410bd4-cc87-442a-93ff-93c5471bfe5c)
parseBAtom ts =
  case parseATerm ts of
    Right (lhs, TEqual:ts') ->
      case parseATerm ts' of
        Right (rhs, ts'') -> Right (Equal lhs rhs, ts'')
        Left e -> Left e
    -- Then I just matched the same pattern for Unequal
    Right (lhs, TUnequal:ts') ->
      case parseATerm ts' of
        Right (rhs, ts'') -> Right (Not (Equal lhs rhs), ts'')
        Left e -> Left e
    Right (_, ts') -> Left $ "expected = after expression; found: " ++ show ts'
    Left e -> Left e

tryParseBTerm :: String -> BExp AExp
tryParseBTerm = tryParse parseBTerm -- this is just defining the function to be a function applied to a function

-- ==================================================

-- Used claude for the beginning of the stmts (https://claude.ai/chat/27410bd4-cc87-442a-93ff-93c5471bfe5c)
parseStmt :: [Token] -> Either String (Stmt AExp BExp, [Token])
parseStmt (TSkip:ts) = Right (Skip, ts)
parseStmt (TId id:TAssign:ts) =
  case parseATerm ts of
    Right (e, ts') -> Right (Assign id e, ts')
    Left e -> Left e
parseStmt (TIf:ts) = -- also used claude here for If
  case parseBTerm ts of
    Right (cond, TThen:ts') ->
      case parseStmt ts' of
        Right (thenBranch, TElse:ts'') ->
          case parseStmt ts'' of
            Right (elseBranch, TEnd:ts''') -> Right (If cond thenBranch elseBranch, ts''')
            Right (_, ts''') -> Left $ "expected end; found: " ++ show ts'''
            Left e -> Left e
        Right (_, ts'') -> Left $ "expected else; found: " ++ show ts''
        Left e -> Left e
    Right (_, ts') -> Left $ "expected then; found: " ++ show ts'
    Left e -> Left e
-- Once I got If, I was able to pattern match on my own to implement While
parseStmt (TWhile:ts) = 
  case parseBTerm ts of
    Right (cond, TDo: ts') ->
      case parseStmt ts' of
        Right (thenBranch, TEnd:ts'') -> Right (While cond thenBranch, ts'')
        Right (_, ts'') -> Left $ "expected end; found: " ++ show ts''
        Left e -> Left e
    Right (_, ts') -> Left $ "expected do; found: " ++ show ts'
    Left e -> Left e
parseStmt ts = Left $ "expected statement; found: " ++ show ts

tryParseStmt :: String -> Stmt AExp BExp
tryParseStmt = tryParse parseStmt

-- ==================================================

run :: Store -> String -> Store
run store str = eval store (tryParseStmt str)

-- Code from Previous assignment
evalA :: Store -> AExp -> Int
evalA (Store s) (Var x) = fromMaybe 0 (Map.lookup x s)
evalA _ (Num n) = n
evalA s (Plus x y) = (evalA s x) + (evalA s y)
evalA s (Times x y) = (evalA s x) * (evalA s y)
evalA s (Neg x) = (-1) * (evalA s x)

evalB :: Store -> BExp AExp -> Bool
evalB _ (Bool b) = b
evalB s (Equal x y) = (evalA s x) == (evalA s y)
evalB s (Lt x y) = (evalA s x) < (evalA s y)
evalB s (Not b) = not (evalB s b)
evalB s (Or b c) = (evalB s b) || (evalB s c)
evalB s (And b c) = (evalB s b) && (evalB s c)

eval :: Store -> Stmt AExp BExp -> Store
eval s Skip = s
eval (Store s) (Assign x y) = Store (Map.insert x (evalA (Store (s)) y) s)
eval s (Seq st1 st2) = eval (eval s st1) st2
eval s (If b st1 st2) = if (evalB s b) then (eval s st1) else (eval s st2)
eval s w@(While b st1) = if (evalB s b) then eval (eval s st1) w else s


-- Tests =================================================================

main = do

    putStrLn "\nPart 1: Test existing code ---------------------------------------------------------"
    putStr "\nShould be Plus (Var \"x\") (Times (Num 10) (Num 3))\n          "
    print $ tryParseATerm "x + 10 * 3"

    putStrLn "\nPart 2: Test Boolean Expression parsing --------------------------------------------"
    putStr "\nShould be Or (Bool False) (And (Bool True) (Bool False))\n          "
    print $ tryParseBTerm "false OR true AND false"

    putStr "\nShould be Equal (Var \"x\") (Num 0)          \n          "
    print $ tryParseBTerm "x = 0"

    putStrLn "\nPart 3: Test Statement parsing -----------------------------------------------------"
    putStr "\nShould be Skip\n          "
    print $ tryParseStmt "SKIP"

    putStr "\nShould be Assign \"x\" (Num 5)\n          "
    print $ tryParseStmt "x := 5"

    putStr "\nShould be If (Not (Equal (Var \"x\") (Num 0))) "
    putStr "(Assign \"y\" (Num 10)) (Assign \"y\" (Var \"x\"))\n          "
    print $ tryParseStmt "IF x != 0 THEN y := 10 ELSE y := x END"

    putStr "\nShould be While (Bool True) Skip       \n          "
    print $ tryParseStmt "WHILE true DO SKIP END"

    putStrLn "\nPart 4: Test parsing with evaluation -----------------------------------------------"

    --let store = Map.fromList[("a", 2), ("b", 3)]
    -- Again, I had to just put a Store ( ) wrapper to account for the artifact from Hw03
    let store = Store (Map.fromList[("a", 2), ("b", 3)])

    putStr "\nShould be fromList [(\"a\",2),(\"b\",10)]\n          "
    print $ run store "WHILE b != 10 DO b := b + 1 END"

