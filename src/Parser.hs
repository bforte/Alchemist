{-# LANGUAGE FlexibleContexts #-}

module Parser ( parseInput, parseProg ) where

import Data.Maybe
import Text.Parsec

import Eval

type Parsed a = Either ParseError a
type Parser a = Parsec String () a


-- | Parse a complete program; ie. multiple rules (LHS,RHS) ignoring comments
parseProg :: String -> Parsed (Prog,Inputs)
parseProg = parse (progP <* eof) "src"  where

  progP = (,) . catMaybes <$> lineP `sepEndBy` cNewline
                          <*> (inputsP <* eof <|> [] <$ eof)

  -- Parsing the program
  lineP = Just <$> ruleP <|> comment

  ruleP = (,) <$> (spaces *> lhsP) <*> (rSepP *> rhsP <* spaces')

  lhsP = multi (identP' <* spaces') `sepBy` iSepP
  rhsP = multi (identP  <* spaces') `sepBy` iSepP

  rSepP = string' "->"
  iSepP = string' "+"

  multi p = (,) <$> (numberP <|> pure 1) <*> (spaces' *> p)

  -- Parsing possible inputs separated by bang
  inputsP = char '!' *> inputP `sepEndBy` many1 space


-- | Parse a single command-line argument of the form IDENT:NUMBER
parseInput :: Integer -> String -> Parsed (String,Integer)
parseInput = parse (inputP <* eof) . ("arg-"++) . show

inputP = (,) <$> identP' <*> (char ':' *> numberP)

{- Some more general parsers -}

comment :: Monoid m => Parser m
comment = mempty <$ char '#' <* many (noneOf "\n")

cNewline :: Parser ()
cNewline = () <$ newline <|> (comment <* newline)

quoted :: Parser String
quoted = between (char '"') (char '"') (many $ unescaped <|> escaped) where
  unescaped = noneOf "\\\""
  escaped = char '\\' *> oneOf "\\\""

spaces' :: Parser ()
spaces' = () <$ many (oneOf "\v\t\f ")

string' :: String -> Parser String
string' s = string s <* spaces'

numberP :: Parser Integer
numberP = read <$> many1 digit

identP :: Parser Ident
identP =  In  <$> suffixP "In_" identP'
      <|> Out <$> suffixP "Out_" (identP' <|> quoted)
      <|> Id  <$> identP'
  where suffixP s p = try (string s) *> p

identP' :: Parser String
identP' = (:) <$> nd <*> many (nd <|> digit)  where
  nd = letter <|> char '_'
