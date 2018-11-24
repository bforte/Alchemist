
module Eval ( Ident(..), Input, Prog, runProg ) where

import Control.Monad.State.Strict
import Data.Foldable
import Data.List
import Data.Map.Strict (Map, alter, findWithDefault, fromList, update)
import System.Random


type Input = (String,Integer)
type Prog  = [Rule]
type Rule  = (LHS,RHS)
type LHS = [(Integer,String)]
type RHS = [(Integer,Ident)]

type Universe = Map String Integer

data Ident = In String | Out String | Id String
  deriving (Show,Eq)


runProg :: (Prog,[Input]) -> IO Universe
runProg (prog,xs) = execStateT loop (fromList xs)  where
  loop = do
    st <- get
    case filter (applicable st) prog of
      [] -> pure ()
      rs -> do
        r <- liftIO $ (rs !!) <$> randomRIO (0,length rs-1)
        st' <- liftIO $ apply r st
        put st'
        loop


applicable :: Universe -> Rule -> Bool
applicable xs (lhs,_) = and [ c <= count i xs | (c,i) <- lhs ]

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

  readINT = pure 1

  readNIntegers n
    | n <= 0    = pure 0
    | otherwise = (+) <$> readNIntegers (n-1) <*> readINT

count :: String -> Universe -> Integer
count = findWithDefault 0
