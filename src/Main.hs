
module Main where

import Control.Monad
import Data.Function
import Data.List
import Data.Map.Strict (assocs)
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Random

import Eval
import Parser


type Add a = a -> a -> a

--   Flags :       expr  add          seed          debug help
data Flags = Flags Bool (Add Inputs) (Maybe String) Bool  Bool
defaults = Flags False merge Nothing False False where
  merge a b =
    [ (i,sum cs)
    | (i:_,cs) <- map unzip . groupBy ((==) `on` fst) $ a ++ b
    ]

options =
  [ Option "e" ["expression"] (NoArg expr) "evaluate expression"
  , Option "o" ["override"] (NoArg add) "override constant inputs (merges by default)"
  , Option "s" ["seed"] (ReqArg seed "SEED") "set seed"
  , Option "d" ["debug"] (NoArg debug) "print the final state to stderr"
  , Option "h" ["help"] (NoArg help) "print this help"
  ] where
    expr  (Flags _ a s d h) = Flags True a s d h
    add   (Flags e _ s d h) = Flags e seq s d h
    seed s(Flags e a _ d h) = Flags e a (Just s) d h
    debug (Flags e a s _ h) = Flags e a s True h
    help  (Flags e a s d _) = Flags e a s d True

usage = usageInfo "usage: cnr [-h] [-s SEED] (-e EXPR | FILE) INPUTS" options


main = getOpt Permute options <$> getArgs >>= \case
  (args,xs,[]) -> runMain xs $ foldr ($) defaults args
  (_,_,err)    -> die $ concat err

die = fail . (++usage) . (++"\n")

runMain _  (Flags _ _ _ _ True) = putStrLn usage
runMain [] (Flags e _ _ _ _) =
  die . ("missing "++) $ if e then "file" else "expression"
runMain (x:xs) (Flags e a s d _) = do
  src <- if e then pure x else readFile x
  maybe mempty (setStdGen . read) s
  hPutStrLn stderr . ("seed: "++) . show . show =<< getStdGen
  out <- either (fail . ('\n':) . show) runProg $ do
    (prog,us) <- parseProg src
    vs <- zipWithM parseInput [1..] xs
    pure (prog, us `a` vs)
  when d $ do
    hFlush stdout
    hPutStrLn stderr "\n---------- Remaining Universe----------"
    hPutStrLn stderr . unlines
                     $ sort ["  " ++ i ++ ": " ++ show c | (i,c) <- assocs out ]
