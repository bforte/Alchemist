{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, ViewPatterns #-}

module Eval (optimize, runProg, unify, Debug(..), Ident(..), Inputs, Prog, Rule, LHS, RHS) where

import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Map.Strict (Map, alter, findWithDefault, fromList, update)
import System.IO
import System.Random

import qualified Data.Set as S


type Inputs = LHS
type Prog  = [Rule]
type Rule  = (LHS,RHS)
type LHS = [(Integer,String)]
type RHS = [(Integer,Ident)]

type Universe = Map String Integer


data Debug = D0 | D1 | D2 | DF String
  deriving Eq

data Ident = In String | OutStr String | OutNum String | Id String
  deriving (Eq,Ord)

instance Show Ident where
    show (Id s) = s
    show (OutNum s) = "Out_" ++ s
    show (OutStr s) = "Out_" ++ show s
    show (In s) = "In_" ++ s

instance {-# OVERLAPS #-} Show Rule where
  show (l,r) = sl l ++ " -> " ++ sr r where
    sl x = intercalate " + " [ s n ++ i | (n,i) <- x]
    sr x = intercalate " + " [ s n ++ show i | (n,i) <- x]
    s 1 = ""
    s n = show n

runProg :: Debug -> (Prog,Inputs) -> IO (Bool,Universe)
runProg d (prog,xs) =
    runProg' (d == D2) (optimize [(unify l,unify' r) | (l,r) <- prog] xs) xs
  where unify' x -- Only unify rules w/o side-effects
          | all noSideEffect x = unify x
          | otherwise = x

        noSideEffect (snd -> OutNum _) = False
        noSideEffect (snd -> OutStr _) = False
        noSideEffect _ = True

unify :: Ord b => [(Integer, b)] -> [(Integer, b)]
unify x = [ (sum cs,i) | (cs,i:_) <- map unzip . groupBy ((==) `on` snd)
                                               $ sortOn snd x
          ]

-- TODO: Bogus rules like "xx -> xx" are not removed
optimize :: Prog -> Inputs -> Prog
optimize p xs = go p $ optimize' p
  where go p p'
          | p == p'   = p
          | otherwise = go p' $ optimize' p'

        optimize' p = filter (not . unreachableRule . fst) p
          where
            unreachableRule :: LHS -> Bool
            unreachableRule = any (\(c,a)-> c > 0 && a `S.notMember` reachableAtoms)

            reachableAtoms = S.fromList $
                map snd xs ++ concatMap (mapMaybe (reachable . snd) . snd) p

            reachable (Id s) = Just s
            reachable (In s) = Just s
            reachable _ = Nothing


runProg' :: Bool -> Prog -> Inputs -> IO (Bool,Universe)
runProg' verbose prog xs = execStateT loop (True,universe)  where
  loop = do
    st <- gets snd
    case filter (applicable st) prog of
      [] -> pure ()
      rs -> do
        when (length rs > 1) $
          puts first False
        r <- liftIO $ (rs !!) <$> randomRIO (0,length rs-1)
        when verbose $
          liftIO (hPrint stderr r)
        st' <- liftIO $ apply r st
        puts second st'
        loop

  universe = fromList [(b,a)|(a,b)<-xs]

puts :: MonadState s m => ((y -> x) -> s -> s) -> x -> m ()
puts f = modify . f . const

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
  alterM m (n,OutNum v) = m <$ putStr (concat $ genericReplicate n z)
    where z = show $ findWithDefault 0 v m
  alterM m (n,OutStr v) = m <$ putStr (concat $ genericReplicate n v)
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
