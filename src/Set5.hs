{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude

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

makeRandom :: Gen Integer
makeRandom = Gen rand

fiveRands :: Gen [Integer]
fiveRands = sequence $ replicate 5 makeRandom

randLetter :: Gen Char
randLetter = liftM toLetter makeRandom

randString3 :: Gen String
randString3 = sequence $ replicate 3 randLetter

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair genA genB = liftM2 (,) genA genB


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
maximumMay []     = Nothing
maximumMay (a:as) = return $ go a as
  where go max []       = max
        go max (a:as) 
          | max >= a    = go max as
          | otherwise   = go a as

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []     = Nothing
minimumMay (a:as) = return $ go a as
  where go min []       = min
        go min (a:as) 
          | min <= a    = go min as
          | otherwise   = go a as

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gr str = do
  xs <- lookupMay str gr
  tail <- tailMay xs
  max <- maximumMay tail
  head <- headMay xs
  divMay (fromIntegral max) (fromIntegral head)

salaries :: [(String, Integer)]
salaries = [ ("alice", 105000)
           , ("bob", 90000)
           , ("carol", 85000)
           ]
addSalaries :: [(String, Integer)] 
            -> String 
            -> String
            -> Maybe Integer
addSalaries salaries name1 name2 = do
  salary1 <- lookupMay name1 salaries
  salary2 <- lookupMay name2 salaries
  return $ salary1 + salary2

tailProd :: Num a => [a] -> Maybe a
tailProd list = do
  tail <- tailMay list
  return $ product tail

tailSum :: Num a => [a] -> Maybe a
tailSum list = do
  tail <- tailMay list
  return $ sum tail

tailMax :: Ord a => [a] -> Maybe a
tailMax list = do
  tail <- tailMay list
  max <- maximumMay tail
  return max


-- Set 3

data Card = Card Int String

instance Show Card where
  show (Card int str) = show int ++ str

instance Monad [] where
  return a = [a]

  [] >>= f      = []
  (a:as) >>= f  = (f a) ++ (as >>= f)

allPairs :: [a] -> [b] -> [(a, b)]
allPairs as bs = do
  a <- as
  b <- bs
  return $ (a, b)

allCards :: [Int] -> [String] -> [Card]
allCards ints strs = do
  int <- ints
  str <- strs
  return $ Card int str

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = do
  a <- as
  b <- bs
  c <- cs
  return $ f a b c
