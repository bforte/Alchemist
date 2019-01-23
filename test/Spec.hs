
module Main where

--import Control.Exception (evaluate)
import Data.Either
import Test.Hspec
--import Test.QuickCheck
import Text.Parsec

import Parser


main :: IO ()
main = mapM_ hspec
  [rhsSpec, lhsSpec, ruleSpec, programSpec]

{- Program = Rules? ('!' LHS)?
   Rules   = Rule ('\n' Rule)*
-}
programSpec :: Spec
programSpec = describe "Parser.program" $ do
  it "allows empty programs" $ do
    accepts prog ""
    accepts prog "\n"
    accepts prog "\n\v\t\n\n"
    accepts prog "# empty prog"
    accepts prog "# empty prog\n"
    accepts prog "->"
    accepts prog "->\n"
    accepts prog "# empty prog\n->"
    accepts prog "# empty prog\n->\n"

  it "allows comments" $ do
    accepts prog "a -> b    # comment"
    accepts prog "a -> b    # comment\n"
    accepts prog "# hello\n\n_ -> Out"
    accepts prog "# hello\n\n_ -> Out\n"
    accepts prog "a->b\n#comment######"
    accepts prog "a->b\n#comment######\n"

  it "allows constant inputs" $ do
    accepts prog "a->b  \n!a+b"
    accepts prog "a->b  \n! a+b"
    accepts prog "a->b  \n! a+b\n"
    accepts prog "a->b! a+b\n"
    accepts prog "a->b! 13 a+b\n"
    accepts prog "a->b  \n!a+b\n"

-- Rule = LHS? '->' RHS?
ruleSpec :: Spec
ruleSpec = describe "Parser.rule" $ do

  it "accepts empty rule" $
    accepts rule "->"

  it "doesn't allow I/O atoms on LHS" $ do
    fails rule "Out_x+x->yy"
    fails rule "Out_\"x\"+x->yy"
    fails rule "In_x+x->yy"
    fails rule "x + In_x -> yy"

  it "allows I/O atoms on RHS" $ do
    accepts rule "Outx + foo ->Out_\"\""
    accepts rule "Outx + foo ->In_x+12y+Out_y"

  it "accepts whitespaces" $ do
    accepts rule "x -> x"
    accepts rule "y+x -> x"
    accepts rule "y + x -> x"

  it "accepts multiple atoms" $ do
    accepts rule "21x -> x"
    accepts rule "1x -> 2x"
    accepts rule "x + 31y-> 2x"

-- LHS  = Number? SimpleAtom ('+' Number? SimpleAtom)*
lhsSpec :: Spec
lhsSpec = do

  sideSpec "left" lhs

  describe "Parser.lhs" $
    it "doesn't allow I/O atoms" $ do
      fails lhs "Out_x"
      fails lhs "Out__"
      fails lhs "Out_0"
      fails lhs "In_x"
      fails lhs "In__"
      fails lhs "In_0"
      fails lhs "Out_\"x\""
      fails lhs "Out_\"_\""
      fails lhs "a+Out_a"
      fails lhs "a+2Out_a"

rhsSpec :: Spec
rhsSpec = do

  sideSpec "right" rhs

  describe "Parser.rhs" $ do
    it "allows weird atoms" $ do
      accepts rhs "Outx"
      accepts rhs "Inx"
      accepts rhs "_In_"

    it "doesn't allow empty names for I/O" $ do
      fails rhs "Out_"
      fails rhs "Out_+2y"
      fails rhs "In_"
      fails rhs "x+2In_"

-- RHS  = Number? Atom ('+' Number? Atom)*
sideSpec :: Show a => String -> Parser a -> Spec
sideSpec s p = describe ("Parser.side(" ++ s ++ ")") $ do

  it "doesn't allow numbers" $ do
    fails p "0"
    fails p "123"

  it "accepts multiple atoms" $ do
    accepts p "x+y+x"
    accepts p "x+y+7x"
    accepts p "1x+3y+x"

  it "accepts weird atoms" $ do
    accepts p "___A"
    accepts p "Out"
    accepts p "In"
    accepts p "_Out_foo"
    accepts p "_In_foo"
    accepts p "_0a0_2_A"

  it "allows spaces" $ do
    accepts p "x\v+ 12y"
    accepts p "x  +12y"
    accepts p "x \t+  12y"
    accepts p "x +12 y "
    accepts p "1 x +12y"

  it "doesn't accept newlines" $
    fails p "x +\n3f"


accepts :: Show a => Parser a -> String -> Expectation
accepts = parserTest isRight

fails :: Show a => Parser a -> String -> Expectation
fails = parserTest isLeft

parserTest :: Show a =>
  (Either ParseError a -> Bool) -> Parser a -> String ->
  Expectation
parserTest f p s = shouldSatisfy (parse (p <* eof) "hunit" s) f
