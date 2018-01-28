module Set4Redo where

import MCPrelude
import Prelude hiding 
  ( Monad
  , return
  , bind
  , (>>=)
  , sequence
  , fmap
  , Maybe
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

-- instance Show a => Show (Maybe a) where
--   show Nothing  = "Nothing"
--   show (Just a) = "Just " ++ show a

-- headMay :: [a] -> Maybe a
-- headMay []      = Nothing
-- headMay (a:as)  = Just a

-- tailMay :: [a] -> Maybe [a]
-- tailMay []      = Nothing
-- tailMay (a:as)  = Just as

-- lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
-- lookupMay _ []            = Nothing
-- lookupMay a ((a', b):xs)  = 
--   if (a == a') 
--      then Just b
--      else lookupMay a xs

-- divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
-- divMay top 0    = Nothing
-- divMay top bot  = Just $ top / bot

-- maximumMay :: Ord a => [a] -> Maybe a
-- maximumMay []   = Nothing
-- maximumMay list = Just $ go (head list) list
--   where go max []     = max
--         go max (a:as) 
--           | max >= a    = go max as
--           | otherwise   = go a as

-- minimumMay :: Ord a => [a] -> Maybe a
-- minimumMay []   = Nothing
-- minimumMay list = Just $ go (head list) list
--   where go min []     = min
--         go min (a:as) 
--           | min <= a    = go min as
--           | otherwise   = go a as

-- queryGreek :: GreekData -> String -> Maybe Double
-- queryGreek gr str =
--   case lookupMay str gr of
--     Nothing   -> Nothing
--     Just xs   ->
--       case tailMay xs of
--         Nothing   -> Nothing
--         Just tail -> 
--           case maximumMay tail of
--             Nothing   -> Nothing
--             Just max  ->
--               case headMay xs of
--                 Nothing   -> Nothing
--                 Just head ->
--                   case divMay (fromIntegral max)
--                               (fromIntegral head) of
--                     Nothing -> Nothing
--                     result  -> result

-- chain :: (a -> Maybe b) -> Maybe a -> Maybe b
-- chain transformer mayA =
--   case mayA of
--     Nothing -> Nothing
--     Just a  -> transformer a

-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- link mayA transformer = flip chain mayA transformer

-- queryGreek2 :: GreekData -> String -> Maybe Double
-- queryGreek2 gr str =
--   link (lookupMay str gr) $ \xs -> 
--     link (tailMay xs) $ \tail ->
--       link (maximumMay tail) $ \max ->
--         link (headMay xs) $ \head ->
--           divMay (fromIntegral max) (fromIntegral head)

-- salaries :: [(String, Integer)]
-- salaries = [ ("alice", 105000)
--            , ("bob", 90000)
--            , ("carol", 85000)
--            ]

-- addSalaries :: [(String, Integer)] 
--             -> String 
--             -> String
--             -> Maybe Integer
-- addSalaries salaries name1 name2 =
--   case lookupMay name1 salaries of
--     Nothing       -> Nothing
--     Just salary1  ->
--       case lookupMay name2 salaries of
--         Nothing       -> Nothing
--         Just salary2  -> 
--           mkMaybe $ salary1 + salary2

-- yLink :: (Eq a, Num b) => [(a, b)] -> a -> a -> Maybe b
-- yLink list a1 a2 =
--   link (lookupMay a1 list) $ \b1 ->
--     link (lookupMay a2 list) $ \b2 ->
--       mkMaybe $ b1 + b2

-- addSalaries2 :: [(String, Integer)] 
--              -> String 
--              -> String
--              -> Maybe Integer
-- addSalaries2 salaries name1 name2 =
--   yLink salaries name1 name2

-- mkMaybe :: a -> Maybe a
-- mkMaybe = Just

-- tailProd :: Num a => [a] -> Maybe a
-- tailProd list =
--   link (tailMay list) $ \tail ->
--     mkMaybe $ product tail

-- tailSum :: Num a => [a] -> Maybe a
-- tailSum list =
--   link (tailMay list) $ \tail ->
--     mkMaybe $ sum tail

-- transMaybe :: Num a => (a -> b) -> Maybe a -> Maybe b
-- transMaybe transformer mayA =
--   link mayA $ \a -> mkMaybe $ transformer a

-- tailProd2 :: (Foldable t, Num (t a), Num a) => t a -> Maybe a
-- tailProd2 list = transMaybe product (mkMaybe list)

-- tailSum2 :: (Foldable t, Num (t a), Num a) => t a -> Maybe a
-- tailSum2 list = transMaybe sum (mkMaybe list)

-- tailMax :: (Ord a, Num a, Num [a]) => [a] -> Maybe (Maybe a)
-- tailMax list = 
--   transMaybe maximumMay (mkMaybe list)

-- tailMin :: (Ord a, Num a, Num [a]) => [a] -> Maybe (Maybe a)
-- tailMin list = 
--   transMaybe minimumMay (mkMaybe list)

-- combine :: Maybe (Maybe a) -> Maybe a
-- combine Nothing           = Nothing
-- combine (Just (Nothing))  = Nothing 
-- combine (Just (Just a))   = Just a
