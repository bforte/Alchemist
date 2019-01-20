{-# LANGUAGE FlexibleContexts #-}

module Parser ( parseInput, parseProg ) where

import Data.Bifunctor
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.Language (haskell)
import Text.Parsec.Token    (stringLiteral)

import Eval

type Parsed a = Either String a
type Parser a = Parsec String () a


-- | Parse a complete program; ie. multiple rules (LHS,RHS) ignoring comments
parseProg :: String -> Parsed (Prog,Inputs)
parseProg = parse' (progP <* eof) "src"  where

  -- Parsing the program
  progP = (,) . catMaybes <$> lineP `sepEndBy` cNewline
                          <*> (constantInputs <* eof' <|> [] <$ eof')

  -- Parsing possible inputs separated by bang
  constantInputs = char '!' *> spaces *> lhsP

  eof' = many cNewline >> eof


lineP :: Parser (Maybe Rule)
lineP = Just <$> ruleP <|> comment
  where ruleP = (,) <$> (spaces *> lhsP) <*> (rSepP *> rhsP <* spaces')

lhsP :: Parser LHS
lhsP = multi' (identP' <* spaces') `sepBy` iSepP

rhsP :: Parser RHS
rhsP = multi  (identP  <* spaces') `sepBy` iSepP

rSepP, iSepP :: Parser String
rSepP = string' "->"
iSepP = string' "+"

multi' :: Parser String -> Parser (Integer, String)
multi' p = multi p >>= \case
  (i,s) | "In_" `isPrefixOf` s || "Out_" `isPrefixOf` s
          -> fail $ "invalid atom: '" ++ s ++ "'"
        | otherwise -> pure (i,s)

multi :: Parser a -> Parser (Integer, a)
multi p = (,) <$> (numberP <|> pure 1) <*> (spaces' *> p)

-- | Parse a single command-line argument of the form IDENT:NUMBER
parseInput :: Integer -> String -> Parsed Inputs
parseInput = parse' (lhsP <* eof) . ("arg-"++) . show


parse' :: Parser a -> SourceName -> String -> Parsed a
parse' p s = first (pretty . show) . parse p s  where
  pretty = ('\n':) . (++"\n") . concatMap ("  "++) . lines


{- Some more general parsers -}

comment :: Monoid m => Parser m
comment = mempty <$ char '#' <* many (noneOf "\n")

cNewline :: Parser ()
cNewline = () <$ newline <|> (comment <* newline)

spaces' :: Parser ()
spaces' = () <$ many (oneOf "\v\t\f ")

string' :: String -> Parser String
string' s = string s <* spaces'

numberP :: Parser Integer
numberP = read <$> many1 digit

identP :: Parser Ident
identP =  In  <$> suffixP "In_" identP'
      <|> suffixP "Out_" (OutNum <$> identP' <|> OutStr <$> stringLiteral haskell)
      <|> Id  <$> identP'
  where suffixP s p = try (string s) *> p

identP' :: Parser String
identP' = (:) <$> nd <*> many (nd <|> digit)  where
  nd = letter <|> char '_'
