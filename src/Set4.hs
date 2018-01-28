module Set4 where

import MCPrelude
  ( Seed
  , mkSeed
  , rand
  , toLetter
  )
import Prelude hiding 
  ( Monad
  , Maybe
  , Just
  , Nothing
  )

type Gen a = Seed -> (a, Seed)

mkGen :: a -> Gen a
mkGen a = \seed -> (a, seed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA modifier rand =
  \seed ->
    let (a, seed') = rand seed
     in (modifier a, seed')

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo rand transformer =
  \seed ->
    let (a, seed') = rand seed
     in transformer a seed'

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 func randA randB =
  genTwo randA $ \a ->
    genTwo randB $ \b ->
      mkGen $ func a b

repRandom :: [Gen a] -> Gen [a]
repRandom []        = mkGen []
repRandom (ga:gas)  = do
  genTwo ga $ \a ->
    genTwo (repRandom gas) $ \as ->
      mkGen $ a : as

class Monad m where
  return :: a -> m a
  bind :: m a -> (a -> m b) -> m b

data Maybe a
  = Just a
  | Nothing

newtype Gen' a = Gen' { runGen :: Seed -> (a, Seed) }

evalGen' :: Gen' a -> Seed -> a
evalGen' genA seed = 
  let (a, _) = runGen genA seed
   in a

instance Monad [] where
  -- return :: a -> [a]
  return a = [a]

  -- bind :: [a] -> (a -> [b]) -> [b]
  bind [] _         = []
  bind (a:as) func  =
    func a ++ bind as func

instance Monad Maybe where
  -- return :: a -> Maybe a
  return = Just

  -- bind :: Maybe a -> (a -> Maybe b) -> Maybe b
  bind Nothing _      = Nothing
  bind (Just a) func  = func a

instance Monad Gen' where
  -- return :: a -> Gen' a
  return a = Gen' $ \s -> (a, s)

  -- bind :: Gen' a -> (a -> Gen' b) -> Gen' b
  bind genA func =
    Gen' $ \s -> 
      let (a, s') = runGen genA s
       in runGen (func a) s'

