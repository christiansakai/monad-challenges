{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Set3 where

import MCPrelude 

allPairs :: [a] -> [b] -> [(a, b)]
allPairs [] _       = []
allPairs _ []       = []
allPairs (a:as) bs  =
  map (\b -> (a, b)) bs ++ allPairs as bs

data Card = Card Int String

instance Show Card where
  show (Card int str) = show int ++ str

allCards :: [Int] -> [String] -> [Card]
allCards [] _             = []
allCards _ []             = []
allCards (int:ints) strs  =
  map (\str -> Card int str) strs ++ allCards ints strs

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _               = []
allCombs _ _ []               = []
allCombs combinator (a:as) bs =
  map (\b -> combinator a b) bs ++ allCombs combinator as bs
  
allPairs2 :: [a] -> [b] -> [(a, b)]
allPairs2 = allCombs (,)

allCards2 :: [Int] -> [String] -> [Card]
allCards2 = allCombs Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 combinator as bs cs =
  allCombs (\cToD c -> cToD c) (allCombs combinator as bs) cs
  
combStep :: [a -> b] -> [a] -> [b]
combStep [] _       = []
combStep _ []       = []
combStep (f:fs) as  =
  map (\a -> f a) as ++ combStep fs as

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' func as bs = 
  let fs = map func as
   in combStep fs bs

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' func as bs cs =
  let fas = map func as
      fabs = combStep fas bs
   in combStep fabs cs
