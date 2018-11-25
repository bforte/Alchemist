{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Eval ( Ident(..), Inputs, Prog, runProg ) where

import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Map.Strict (Map, alter, findWithDefault, fromList, update)
import System.IO
import System.Random


type Inputs = [(String,Integer)]
type Prog  = [Rule]
type Rule  = (LHS,RHS)
type LHS = [(Integer,String)]
type RHS = [(Integer,Ident)]

type Universe = Map String Integer

data Ident = In String | Out String | Id String
  deriving (Eq,Ord)

instance Show Ident where
    show (Id s) = s
    show (Out s) = "Out_" ++ show s
    show (In s) = "In_" ++ s

instance {-# OVERLAPS #-} Show Rule where
  show (l,r) = sl l ++ " -> " ++ sr r where
    sl x = intercalate " + " [ s n ++ i | (n,i) <- x]
    sr x = intercalate " + " [ s n ++ show i | (n,i) <- x]
    s 1 = ""
    s n = show n

runProg :: Bool -> (Prog,Inputs) -> IO Universe
runProg d (prog,xs) = do
    hPutStrLn stderr . ("seed: "++) . show . show =<< getStdGen
    runProg' d [(unify l, unify r) | (l,r) <- prog] xs
  where unify x = [ (sum cs,i)
                  | (cs,i:_) <- map unzip . groupBy ((==) `on` snd)
                                          $ sortOn snd x
                  ]

runProg' :: Bool -> Prog -> Inputs -> IO Universe
runProg' d prog xs = execStateT loop (fromList xs)  where
  loop = do
    st <- get
    case filter (applicable st) prog of
      [] -> pure ()
      rs -> do
        r <- liftIO $ (rs !!) <$> randomRIO (0,length rs-1)
        when d . liftIO $ hPrint stderr r
        st' <- liftIO $ apply r st
        put st'
        loop

applicable :: Universe -> Rule -> Bool
applicable xs (lhs,_) = and
  [ if c == 0 then c' == 0 else c <= c'
  | (c,i) <- lhs
  , let c' = count i xs
  ]

apply :: Rule -> Universe -> IO Universe
apply (lhs,rhs) xs = flip (foldlM alterM) rhs
                   $ foldl' (\f (n,v)-> update (sub' n) v f) xs lhs  where

  alterM :: Universe -> (Integer,Ident) -> IO Universe
  alterM m (n,In v) = do
    n' <- readNIntegers n
    pure $ alter (add' n') v m
  alterM m (n,Out v) = m <$ putStr (concat $ genericReplicate n v)
  alterM m (n,Id v) = pure $ alter (add' n) v m

  sub' :: Integer -> Integer -> Maybe Integer
  sub' n x
    | diff <= 0 = Nothing
    | otherwise = Just diff
    where diff = x - n

  add' n (Just x) = Just $ n+x
  add' n Nothing  = Just n

  readNIntegers n
    | n <= 0    = pure 0
    | otherwise = (+) <$> readNIntegers (n-1) <*> readNumber

  readNumber = getLine >>= \case
    "" -> readNumber
    s | all isDigit s -> pure $ read s
      | otherwise -> hPutStrLn stderr "invalid input" >> readNumber


count :: String -> Universe -> Integer
count = findWithDefault 0
