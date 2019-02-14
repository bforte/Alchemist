{-# LANGUAGE FlexibleContexts #-}

module Parser
    ( parseInput, parseProg
    , lhs, rhs, rule, prog, Parser
    ) where

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import Text.Parsec

import Eval

type Parsed a = Either String a
type Parser a = Parsec String () a


parseProg :: String -> Parsed (Prog,Inputs)
parseProg = parse' (prog <* eof) "src"

parseInput :: Integer -> String -> Parsed Inputs
parseInput = parse' (spaces' *> lhs <* eof) . ("arg-"++) . show


parse' :: Parser a -> SourceName -> String -> Parsed a
parse' p s = first (pretty . show) . parse p s  where
  pretty = ('\n':) . (++"\n") . concatMap ("  "++) . lines


{- Alchemist parsers -}

prog :: Parser (Prog,Inputs)
prog = (,) <$> body <*> inputs where
    body   = catMaybes <$> sepEndBy (spaces' *> line <* spaces') eol
    inputs = option [] (char '!' *> spaces *> lhs <* spaces)


line :: Parser (Maybe Rule)
line =  Just <$> rule
    <|> Nothing <$ lineComment
    <|> Nothing <$ lookAhead newline

rule :: Parser Rule
rule = (,) <$> (lhs <* string "->") <*> rhs

lhs :: Parser LHS
lhs = sepByPlus (multiple simpleAtom) where
  simpleAtom = ident >>= \case
    i | "In_" `isPrefixOf` i || "Out_" `isPrefixOf` i
        -> invalidAtom i
      | otherwise -> pure i

rhs :: Parser RHS
rhs = spaces' *> sepByPlus (multiple $ identAtom <|> charAtom) where
    identAtom = ident >>= \case
      i | "In_" `isPrefixOf` i && length i > 3 -> pure . In $ drop 3 i
        | "In_" `isPrefixOf` i  -> invalidAtom i
        | "Out_" `isPrefixOf` i -> out $ drop 4 i
        | "In" `isPrefixOf` i  -> char '\'' >> InC  <$> ident
        | "Out" `isPrefixOf` i -> char '\'' >> OutC <$> ident
        | otherwise -> pure $ Id i

    charAtom = (Dump <$ char '?') <|> (Clear <$ char '%')

    out i@(c:_)
      | isAlpha c || c == '_' = pure $ OutNum i
      | otherwise = invalidAtom i
    out "" = OutStr <$> stringLit


{- Combinators -}

sepByPlus :: Parser a -> Parser [a]
sepByPlus p = sepBy (p <* spaces') (char '+' *> spaces')

multiple :: Parser a -> Parser (Integer,a)
multiple p = (,) <$> option 1 (numberLit <* spaces') <*> p

invalidAtom :: Monad m => String -> m a
invalidAtom = fail . ("invalid atom: '"++) . (++"'")


{- Tokens -}

eol :: Parser ()
eol = option () lineComment <* (() <$ newline <|> eof)

spaces' = skipMany (satisfy isSpace') <?> "white space" where
  isSpace' '\n' = False
  isSpace' c = isSpace c

lineComment :: Parser ()
lineComment = () <$ char '#' <* many (noneOf "\n")

ident :: Parser String
ident = (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')

numberLit :: Parser Integer
numberLit = read <$> many1 digit

-- Parse a quoted string literal, taken from http://hackage.haskell.org/package/parsec-3.1.13.0/docs/src/Text.Parsec.Token.html#makeTokenParser
-- (stringLiteral haskell) won't do the job since it will consume spaces too..
stringLit :: Parser String
stringLit =
  (do str <- between (char '"')
                     (char '"' <?> "end of string")
                     (many stringChar)
      return (foldr (maybe id (:)) "" str)
  ) <?> "literal string"

  where
    stringChar      =   Just <$> stringLetter
                    <|> stringEscape
                    <?> "string character"

    stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape    = do{ _ <- char '\\'
                        ;     do{ _ <- escapeGap  ; return Nothing }
                          <|> do{ _ <- escapeEmpty; return Nothing }
                          <|> Just <$> escapeCode
                        }

    escapeEmpty     = char '&'
    escapeGap       = do{ _ <- many1 space
                        ; char '\\' <?> "end of string gap"
                        }



    -- escape codes
    escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                    <?> "escape code"

    charControl     = do{ _ <- char '^'
                        ; code <- upper
                        ; return (toEnum (fromEnum code - fromEnum 'A' + 1))
                        }

    charNum         = do{ code <- decimal
                                  <|> do{ _ <- char 'o'; number 8 octDigit }
                                  <|> do{ _ <- char 'x'; number 16 hexDigit }
                        ; if code > 0x10FFFF
                          then fail "invalid escape sequence"
                          else return (toEnum (fromInteger code))
                        }

    charEsc         = choice (map parseEsc escMap)
                    where
                      parseEsc (c,code)     = do{ _ <- char c; return code }

    charAscii       = choice (map parseAscii asciiMap)
                    where
                      parseAscii (asc,code) = try (do{ _ <- string asc; return code })


    -- escape code tables
    escMap          = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
    asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                       "FS","GS","RS","US","SP"]
    ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                       "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                       "CAN","SUB","ESC","DEL"]

    ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                       '\EM','\FS','\GS','\RS','\US','\SP']
    ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                       '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                       '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']

    -- integer parsing

    decimal         = number 10 digit

    number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }
