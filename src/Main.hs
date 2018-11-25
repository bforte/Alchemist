
module Main where

import Control.Monad
import Data.Char
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

data Debug = D0 | D1 | D2 | DF String deriving Eq

--   Flags :       expr  add          seed          debug help
data Flags = Flags Bool (Add Inputs) (Maybe String) Debug  Bool
defaults = Flags False merge Nothing D0 False

options =
  [ Option "e" ["expression"] (NoArg expr) "evaluate expression"
  , Option "o" ["override"] (NoArg add) "override constant inputs (merges by default)"
  , Option "s" ["seed"] (ReqArg seed "SEED") "set seed"
  , Option "d" ["debug"] (OptArg debug "LEVEL") "set debug level (0 to 2)"
  , Option "h" ["help"] (NoArg help) "print this help"
  ] where
    expr    (Flags _ a s d h) = Flags True a s d h
    add     (Flags e _ s d h) = Flags e seq s d h
    seed  s (Flags e a _ d h) = Flags e a (Just s) d h
    debug d (Flags e a s _ h) = Flags e a s d' h  where
      d' = case d of
        Nothing -> D1
        Just s | all isDigit s -> case read s of
                   0 -> D0
                   1 -> D1
                   2 -> D2
                   _ -> DF s
               | otherwise -> DF s
    help    (Flags e a s d _) = Flags e a s d True

usage = usageInfo "usage: alchemist [-h] [-s SEED] (-e EXPR | FILE) INPUTS" options


main = getOpt Permute options <$> getArgs >>= \case
  (args,xs,[]) -> runMain xs $ foldr ($) defaults args
  (_,_,err)    -> die $ concat err

die = fail . (++usage) . (++"\n")

runMain _  (Flags _ _ _ _ True) = putStrLn usage
runMain [] (Flags e _ _ _ _) =
  die . ("missing "++) $ if e then "file" else "expression"
runMain _ (Flags _ _ _ (DF d) _) =
  die $ "invalid debug level '" ++ d ++ "'"
runMain (x:xs) (Flags e a s d _) = do
  src <- if e then pure x else readFile x
  maybe mempty (setStdGen . read) s
  out <- either fail (runProg $ d == D2) $ do
    (prog,us) <- parseProg src
    vs <- zipWithM parseInput [1..] xs
    pure (prog, merge [] us `a` merge [] vs)
  when (d /= D0) $ do
    hFlush stdout
    hPutStrLn stderr "\n---------- Remaining Universe----------"
    hPutStrLn stderr . unlines
                     $ sort ["  " ++ i ++ ": " ++ show c | (i,c) <- assocs out ]

merge a b =
  [ (i,sum cs)
  | (i:_,cs) <- map unzip . groupBy ((==) `on` fst)
                          . sortOn fst
                          $ a ++ b
  ]
