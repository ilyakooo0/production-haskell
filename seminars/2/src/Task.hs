{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task
  ( calculate,
    Expr(..),
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

data Expr 
  = Num Int 
  | AddExpr Expr Expr 
  | MulExpr Expr Expr 
  | SubExpr Expr Expr 
  | DivExpr Expr Expr
  deriving Show

half :: Gen a -> Gen a
half = scale (`div` 2)

instance Arbitrary Expr where
  arbitrary = do
    size <- getSize 
    case size of 
      0 -> Num <$> arbitrary 
      otherwise ->
        oneof [
          Num <$> arbitrary,
          AddExpgr <$> half arbitrary <*> half arbitrary,
          SubExpr <$> half arbitrary <*> half arbitrary,
          MulExpr <$> half arbitrary <*> half arbitrary,
          DivExpr <$> half arbitrary <*> half arbitrary
          ]

parse :: Parser Expr
parse = Num <$> number <|> (do
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
   Left msg -> trace msg Nothing
   Right (x, _) -> Just x

calculate :: String -> Maybe Int
calculate s = do
  expression <- runParser parseFull s
  evaluate expression
