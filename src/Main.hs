
module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Map.Strict (assocs)
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Random

import Eval
import Parser


data Flags = Flags
   Bool                          -- evaluate expr?
   (Inputs -> Inputs -> Inputs)  -- how to merge inputs
   (Maybe String)                -- seed
   Debug                         -- debugging level
   Bool                          -- print help?

defaults = Flags False merge Nothing D0 False

merge a b = unify (a++b)

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
  (args,xs,[]) -> runMain xs $ foldr ($) defaults (reverse args)
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
  (det,out) <- either fail (runProg d) $ do
    (prog,us) <- parseProg src
    vs <- concat <$> zipWithM parseInput [1..] xs
    let inputs' = us `a` vs
        inputs
          | any (("_"==).snd) inputs' = inputs'
          | otherwise = (1,"_") : inputs'
    pure (prog, merge [] inputs)
  when (d /= D0) $ do
    hFlush stdout
    hPutStrLn stderr "\n--------------------------------------"
    hPutStrLn stderr . ("seed: "++) . show . show =<< getStdGen
    when det $
      hPutStrLn stderr "The computation was deterministic"
    hPutStrLn stderr "--------- Remaining Universe ---------"
    hPutStrLn stderr . unlines
                     $ sort ["  " ++ i ++ ": " ++ show c | (i,c) <- assocs out ]
