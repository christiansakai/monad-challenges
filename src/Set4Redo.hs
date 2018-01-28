{-# LANGUAGE FlexibleContexts #-}

module Set4Redo where

import MCPrelude
import Prelude hiding 
  ( Monad
  , return
  , bind
  , (>>=)
  , sequence
  , fmap
  , Maybe(..)
  )

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

sequence :: Monad m => [m a] -> m [a]
sequence []       = return []
sequence (ma:mas) =
  ma >>= \a ->
    (sequence mas) >>= \as ->
      return $ a:as

chain :: Monad m => (a -> m b) -> m a -> m b
chain = flip (>>=)

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = 
  ma >>= \a ->
    mf >>= \f ->
      return $ f a

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ap (return f) ma 

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = ap (ap (return f) ma) mb

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = ap (ap (ap (return f) ma) mb) mc

combine :: Monad m => m (m a) -> m a
combine mma = 
  mma >>= \ma -> ma

-- Set 1

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Monad Gen where
  return a  = Gen $ \s -> (a, s)

  ga >>= f  = Gen $ \s ->
    let (a, s') = runGen ga s
     in runGen (f a) s'

randInteger :: Gen Integer
randInteger = Gen rand

randLetter :: Gen Char
randLetter = liftM toLetter randInteger

randEven :: Gen Integer 
randEven = liftM (* 2) randInteger

randOdd :: Gen Integer
randOdd = liftM (+ 1) randInteger

randTen :: Gen Integer
randTen = liftM (* 10) randInteger

randPair :: Gen (Char, Integer)
randPair = liftM2 (,) randLetter randInteger

randList :: [Gen a] -> Gen [a]
randList genAs = sequence genAs 

-- Set 2

data Maybe a
  = Just a
  | Nothing

instance Show a => Show (Maybe a) where
  show Nothing  = "Nothing"
  show (Just a) = "Just " ++ show a

instance Monad Maybe where
  return = Just

  Nothing >>= _ = Nothing
  Just a >>= f  = f a 

headMay :: [a] -> Maybe a
headMay []      = Nothing
headMay (a:as)  = return a

tailMay :: [a] -> Maybe [a]
tailMay []      = Nothing
tailMay (a:as)  = return as

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ []            = Nothing
lookupMay a ((a', b):xs)  = 
  if (a == a') 
     then return b
     else lookupMay a xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay top 0    = Nothing
divMay top bot  = return $ top / bot

maximumMay :: Ord a => [a] -> Maybe a
maximumMay []   = Nothing
maximumMay list = return $ go (head list) list
  where go max []     = max
        go max (a:as) 
          | max >= a    = go max as
          | otherwise   = go a as

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []   = Nothing
minimumMay list = return $ go (head list) list
  where go min []     = min
        go min (a:as) 
          | min <= a    = go min as
          | otherwise   = go a as

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gr str =
  lookupMay str gr >>= \xs ->
    tailMay xs >>= \tail ->
      maximumMay tail >>= \max ->
        headMay xs >>= \head ->
          divMay (fromIntegral max)
                 (fromIntegral head) 

salaries :: [(String, Integer)]
salaries = [ ("alice", 105000)
           , ("bob", 90000)
           , ("carol", 85000)
           ]

addSalaries :: [(String, Integer)] 
            -> String 
            -> String
            -> Maybe Integer
addSalaries salaries name1 name2 =
  liftM2 (+) (lookupMay name1 salaries) (lookupMay name2 salaries)

tailProd :: Num a => [a] -> Maybe a
tailProd list =
  tailMay list >>= \tail -> return $ product tail

tailSum :: Num a => [a] -> Maybe a
tailSum list =
  tailMay list >>= \tail -> return $ sum tail

tailProd2 :: (Foldable t, Num (t a), Num a) => t a -> Maybe a
tailProd2 list = liftM product (return list)

tailSum2 :: (Foldable t, Num (t a), Num a) => t a -> Maybe a
tailSum2 list = liftM sum (return list)

tailMax :: (Ord a, Num a, Num [a]) => [a] -> Maybe (Maybe a)
tailMax list = liftM maximumMay (return list)

tailMin :: (Ord a, Num a, Num [a]) => [a] -> Maybe (Maybe a)
tailMin list = liftM minimumMay (return list)

-- Set 3

data Card = Card Int String

instance Show Card where
  show (Card int str) = show int ++ str

instance Monad [] where
  return a = [a]

  [] >>= f      = []
  (a:as) >>= f  = (f a) ++ (as >>= f)

allCards :: [Int] -> [String] -> [Card]
allCards ints strs = liftM2 Card ints strs
