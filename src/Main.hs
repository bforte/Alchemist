
module Main where

import Control.Monad
import Data.Char
import Data.Map.Strict (assocs)
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Random

import Eval
import Parser

data Flags = Flags Bool Bool Bool (Maybe Int)
defaults = Flags False False False Nothing

options =
  [ Option "e" ["expression"] (NoArg expr) "evaluate expression"
  , Option "s" ["seed"] (ReqArg seed "SEED") "set seed"
  , Option "d" ["debug"] (NoArg debug) "print the final state to stderr"
  , Option "h" ["help"] (NoArg help) "print this help"
  ] where
    expr (Flags h _ d s) = Flags h True d s
    seed s (Flags h e d _) = Flags h e d . Just . foldl ((+).(256*)) 0 $ ord <$> s
    debug (Flags h e _ s) = Flags h e True s
    help (Flags _ e d s) = Flags True e d s

usage = usageInfo "usage: cnr [-h] [-s SEED] (-e EXPR | FILE) INPUTS" options


main = getOpt Permute options <$> getArgs >>= \case
  (args,xs,[]) -> runMain xs $ foldr ($) defaults args
  (_,_,err)    -> die $ concat err

die = ioError . userError . (++usage) . (++"\n")

runMain [] (Flags True _ _ _) = putStrLn usage
runMain [] (Flags _ e _ _) =
  die . ("missing "++) $ if e then "file" else "expression"
runMain (x:xs) (Flags _ e d s) = do
  src <- if e then pure x else readFile x
  maybe mempty (setStdGen . mkStdGen) s
  fin <- either (ioError . userError . show) runProg $
    (,) <$> parseProg src <*> zipWithM parseInput [1..] xs
  when d $ do
    hFlush stdout
    hPutStrLn stderr $ '\n' : show (assocs fin)
