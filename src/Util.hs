
module Util where

import Data.List
import Data.Ord


(<*>) :: Monad m => m (a -> b) -> m a -> m b
mf <*> ma = do
  f <- mf
  a <- ma
  return $ f a


(<*) :: Monad m => m a -> m b -> m a
ma <* mb = do
  a <- ma
  _ <- mb
  return a

(*>) :: Monad m => m a -> m b -> m b
ma *> mb = do
  _ <- ma
  b <- mb
  return b

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
