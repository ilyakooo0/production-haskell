{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task
  ( calculate,
    Expr(..),
    parseFull,
    runParser,
  )
where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Char
import Data.Maybe
import Test.QuickCheck
import Text.Read
import Debug.Trace
import Control.Monad

type ParsingResult a = Either String (a, String)

-- Parser :: Type -> Type
newtype Parser a = Parser (String -> ParsingResult a) 
  deriving Functor

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\q -> Right (x, q))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>)
    (Parser (e :: String -> ParsingResult (a -> b))) 
    (Parser (x :: String -> ParsingResult a)) = Parser (\s -> 
    case e s of
    Left err -> Left err
    Right (arr, sLeft) -> case x sLeft of
        Left err2 -> Left err2
        Right (resOfTypeA, sLeft2) -> Right (arr resOfTypeA, sLeft2))
        
instance Monad Parser where
  return :: a -> Parser a
  return = pure
  
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser x) e = Parser (\s -> 
    case x s of
      Left err -> Left err
      Right (a, s1) -> case e a of
        Parser f -> f s1
    )

instance MonadFail Parser where
  fail :: String -> Parser a
  fail err = Parser . const . Left $ err

instance Alternative Parser where
  empty :: Parser a
  empty = Parser . const . Left $ "I'm empty"

  (Parser left) <|> (Parser right) = Parser $ \s -> case (left s, right s) of
    (Right res, _) -> Right res
    (Left _, Right res) -> Right res
    (Left err1, Left err2) -> Left $ "Both alternatives failed: 1) " ++ err1 ++ "\n2:" ++ err2

anyToken :: Parser Char
anyToken = Parser $ \s -> case s of
  "" -> Left "Empty String"
  (hd: tl) -> Right (hd, tl)

peekToken :: Parser Char
peekToken = Parser $ \s -> case s of
  "" -> Left "Empty String"
  (hd: tl) -> Right (hd, hd : tl)

eos :: Parser ()
eos = Parser $ \s -> case s of
  "" -> Right ((), "")
  s -> Left $ "unexpected trailing content: " ++ s

token :: Char -> Parser ()
-- token expected = Parser $ \s -> case s of
--   "" -> Left "Empty String"
--   (hd: tl) -> if hd == expected then Right ((), tl) else Left  $ 
token expected = do
  actual <- anyToken
  when (actual /= expected) $ fail $ "expected " ++ [expected] ++ ", got " ++ [actual]
-- when :: Applicative f => Bool -> f () -> f ()

tokens :: String -> Parser ()
tokens "" = pure ()
tokens (c: cs) = do
  token c
  tokens cs

-- "x"
-- u <- tokensWhile (== 'a')

tokensWhile :: (Char -> Bool) -> Parser String
-- tokensWhile e = (do
--   char <- anyToken
--   str <- if e char then tokensWhile e else empty
--   pure (char:str)) <|> (pure "")
-- tokensWhile e = (do
--   char <- anyToken
--   str <- if e char then tokensWhile e else empty
--   pure (char:str)) <|> (pure "")

tokensWhile e = tokensWhile1 e <|> pure ""

tokensWhile1 :: (Char -> Bool) -> Parser String
tokensWhile1 e = do
  char <- anyToken
  -- traceShowM char
  str <- if e char then tokensWhile e else empty
  pure (char:str)

-- isDigit :: Char -> Bool

-- TODO: add negative numbers
number:: Parser Int
number = do
  chars <- tokensWhile1 isDigit
  case readMaybe chars of
    Just res -> return res
    Nothing -> fail $ "unexpected invalid number: '" ++ chars ++ "'"

anyNumber :: Parser Int 
anyNumber = do 
  nextChar <- peekToken
  case nextChar of 
    '-' -> do
        anyToken
        number_abs <- number
        pure $ negate number_abs 
    _ -> number

data Expr 
  = Num Int 
  | AddExpr Expr Expr 
  | MulExpr Expr Expr 
  | SubExpr Expr Expr 
  | DivExpr Expr Expr
  deriving (Eq)

instance Show Expr where
  show arg = case arg of
    (Num i) -> show i
    (AddExpr e1 e2) -> template "+" e1 e2
    (MulExpr e1 e2) -> template "*" e1 e2
    (SubExpr e1 e2) -> template "-" e1 e2
    (DivExpr e1 e2) -> template "/" e1 e2
    where template op e1 e2 = "(" <> show e1 <> " " <> op <> " " <> show e2 <> ")"

half :: Gen a -> Gen a
half = scale (`div` 2)

genPrimitive :: Gen Expr
genPrimitive = Num <$> scale ((* 5) . (+ 1)) arbitrary 

genRecursive :: Gen Expr
genRecursive = oneof [
          AddExpr <$> half arbitrary <*> half arbitrary,
          SubExpr <$> half arbitrary <*> half arbitrary,
          MulExpr <$> half arbitrary <*> half arbitrary,
          DivExpr <$> half arbitrary <*> half arbitrary
  ]

recursiveProb :: Int -> (Int, Int)
recursiveProb sz = (maximum [1, sz `div` 5], sz)

instance Arbitrary Expr where
  arbitrary = do
    sz <- getSize
    let (wPrim, wRec) = recursiveProb sz
    frequency [(wPrim, genPrimitive), (wRec, genRecursive)]

parse :: Parser Expr
parse = Num <$> anyNumber <|> (do
    token '('
    tokensWhile isSpace
    expr1 <- parse
    tokensWhile isSpace
    operator <- anyToken  
    tokensWhile isSpace
    expr2 <- parse
    tokensWhile isSpace
    token ')'
    case operator of
      '+' -> return $ AddExpr expr1 expr2
      '*' -> return $ MulExpr expr1 expr2
      '-' -> return $ SubExpr expr1 expr2
      '/' -> return $ DivExpr expr1 expr2
      _ -> fail "unknown operator"
  )

parseFull :: Parser Expr
parseFull = do
  e <- parse
  eos
  return e

evaluate :: Expr -> Maybe Int
evaluate (Num x) = Just x
evaluate (AddExpr left right) = (+) <$> (evaluate left) <*> (evaluate right)
evaluate (MulExpr left right) = (*) <$> (evaluate left) <*> (evaluate right)
evaluate (SubExpr left right) = (-) <$> (evaluate left) <*> (evaluate right)
evaluate (DivExpr _ right) | evaluate right == Just 0 = Nothing
evaluate (DivExpr left right) = div <$> (evaluate left) <*> (evaluate right)


runParser :: Parser x -> String -> Maybe x
runParser (Parser parser_fun) context = 
  case parser_fun context of 
   Left msg -> Nothing
   Right (x, _) -> Just x



calculate :: String -> Maybe Int
calculate s = do
  expression <- runParser parseFull s
  evaluate expression
