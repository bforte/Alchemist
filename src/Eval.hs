{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, TypeSynonymInstances, RecordWildCards, ViewPatterns #-}

module Eval (optimize, runProg, unify, Debug(..), Atom(..), Inputs, Prog, Rule, LHS, RHS) where

import Control.Monad.State.Strict
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import System.IO
import System.Random (randomRIO)

import qualified Data.Set    as S
import qualified Data.Vector as V


type Inputs = LHS
type Prog   = [Rule]
type Rule   = (LHS,RHS)
type LHS    = [(Integer,String)]
type RHS    = [(Integer,Atom)]
type Vect   = V.Vector Integer


data Debug = D0 | D1 | D2 | DF String
  deriving Eq

data Atom = In String | OutStr String | OutNum String | Id String | Clear | Dump
  deriving (Eq,Ord)

instance Show Atom where
    show (Id s) = s
    show (OutNum s) = "Out_" ++ s
    show (OutStr s) = "Out_" ++ show s
    show (In s) = "In_" ++ s
    show Clear = "%"
    show Dump = "?"

instance {-# OVERLAPS #-} Show Rule where
  show (l,r) = sl l ++ " -> " ++ sr r where
    sl x = intercalate " + " [ s n ++ i | (n,i) <- x]
    sr x = intercalate " + " [ s n ++ show i | (n,i) <- x]
    s 1 = ""
    s n = show n

runProg :: Debug -> (Prog,Inputs) -> IO (Bool,String)
runProg d (prog,xs) =
    runProg' (d == D2) (optimize [(unify l,unify' r) | (l,r) <- prog] xs) xs
  where unify' x -- Only unify rules w/o side-effects
          | all noSideEffect x = unify x
          | otherwise = x

data St = S { det :: Bool, univ :: V.Vector Integer, lrule :: Rule, rulec :: Integer }
  deriving Show

runProg' :: Bool -> Prog -> Inputs -> IO (Bool,String)
runProg' verbose prog xs = do
    S{..} <- execStateT loop (S True (lhs xs) ([],[]) 0)
    pure (det, showUniv univ)
  where
    loop = do
      S{..} <- get
      case filter (applicable univ) rules of
        [] -> when verbose $ printRule rulec lrule univ
        rs -> do
          when (length rs > 1) $
            modify (\s-> s { det = False })
          (_,r,f) <- liftIO $ (rs !!) <$> randomRIO (0,length rs-1)
          when verbose $
            if r == lrule then
              modify (\s-> s { rulec = rulec + 1 })
            else do
              printRule rulec lrule univ
              modify (\s-> s { lrule = r, rulec = 1 })
          u <- liftIO $ f univ
          modify (\s-> s { univ = u })
          loop

    rules :: [(Integer, Rule, Vect->IO Vect)]
    !rules = [(n,r,applyRule r) | (n,r) <- zip [1..] prog]

    lhs l = V.fromList $ (\a-> sum [n|(n,x) <- l, a == x]) <$> atoms
    rhs r = V.fromList $ (\a-> sum [n|(n,Id y) <- r, a == y]) <$> atoms

    !atoms = S.elems $ lr2set (xs,[]) <> foldMap lr2set prog
      where lr2set (l,r) = set (snd <$> l) <> set (mapMaybe (reachable' . snd) r)

    atnum a = head [ n | (n,a') <- zip [0..] atoms, a' == a]


    showUniv v = concat
      [ "{"
      , intercalate ", " . zipWith (\a n-> a ++ ": " ++ show n) atoms $ V.toList v
      , "}"
      ]

    applicable u (_,(l,_),_) = not
      $ any (\(n,s)->(if n < 1 then (0/=) else (n>))$ u V.!atnum s) l

    applyRule :: Rule -> Vect -> IO Vect
    applyRule (l,r) u = go (V.zipWith (-) u (lhs l)) $ split r
      where go !z [] = pure z
            go !z (r:rs) = (`go` rs) =<< doIO z r

    doIO :: V.Vector Integer -> (RHS,Maybe(Integer,Atom)) -> IO Vect
    doIO !u (rs,k) = go (V.zipWith (+) u (rhs rs)) k
      where
        go v Nothing = pure v
        go v (Just (n, Id s)) = pure . V.zipWith (+) v $ lhs [(n,s)]
        go v (Just (n, In s)) = do
          ns <- readNIntegers n
          pure . V.zipWith (+) v $ lhs [(ns,s)]
        go v (Just (n, OutStr s)) = v <$ forM_ [1..n] (const $ putStr s)
        go v (Just (n, OutNum s)) = v <$ forM_ [1..n] (const . putStr . show $ v V.! atnum s)
        go v (Just (n, Dump)) = v <$ forM_ [1..n] (const . hPutStrLn stderr $ showUniv v)
        go v (Just (_, Clear)) = pure $ V.map (const 0) v

    printRule c r u = when (c > 0)
      . liftIO
      . hPutStrLn stderr
      $ showUniv u ++ " | " ++ (if c == 1 then "" else show c ++ " times: ") ++ show r


optimize :: Prog -> Inputs -> Prog
optimize p xs = go p $ optimize' p
  where go p p'
          | p == p'   = p
          | otherwise = go p' $ optimize' p'

        optimize' p = filter (not . unreachableRule . fst) p
          where
            unreachableRule :: LHS -> Bool
            unreachableRule l = any (\(c,a)-> c > 0 && a `S.notMember` reachableAtoms l) l

            reachableAtoms l = set $ map snd xs ++ concatMap (rFrom l) p

            rFrom l r
              | l == fst r = []
              | otherwise  = mapMaybe (reachable . snd) $ snd r

split :: RHS -> [(RHS,Maybe (Integer,Atom))]
split = unfoldr go where
  go [] = Nothing
  go r
    | (x,y:z) <- span noSideEffect r = Just ((x,Just y),z)
    | otherwise = Just ((r,Nothing),[])

noSideEffect :: (a,Atom) -> Bool
noSideEffect (snd -> In _) = False
noSideEffect (snd -> OutNum _) = False
noSideEffect (snd -> OutStr _) = False
noSideEffect (snd -> Clear) = False
noSideEffect (snd -> Dump) = False
noSideEffect _ = True

reachable, reachable' :: Atom -> Maybe String
reachable (Id s) = Just s
reachable (In s) = Just s
reachable _ = Nothing
reachable' (OutNum s) = Just s
reachable' x = reachable x


readNIntegers :: Integer -> IO Integer
readNIntegers n
  | n <= 0    = pure 0
  | otherwise = (+) <$> readNIntegers (n-1) <*> readNumber

readNumber :: IO Integer
readNumber = getLine >>= \case
  "" -> readNumber
  s | all isDigit s -> pure $ read s
    | otherwise -> hPutStrLn stderr "invalid input" >> readNumber

set :: Ord a => [a] -> S.Set a
set = S.fromList

unify :: Ord b => [(Integer, b)] -> [(Integer, b)]
unify x = [ (sum cs,i) | (cs,i:_) <- map unzip . groupBy ((==) `on` snd)
                                               $ sortOn snd x
          ]

