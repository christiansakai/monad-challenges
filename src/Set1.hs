{-# LANGUAGE NoImplicitPrelude #-}

module Set1 where

import MCPrelude 

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = 
  let seed = mkSeed 1
      (i1, s1) = rand seed
      (i2, s2) = rand s1
      (i3, s3) = rand s2
      (i4, s4) = rand s3
      (i5, s5) = rand s4
   in [i1, i2, i3, i4, i5]
      
randLetter :: Gen Char
randLetter seed =
  let (num, seed') = rand seed
   in (toLetter num, seed')

randString3 :: String
randString3 = 
  let seed = mkSeed 1
      (c1, s1) = randLetter seed
      (c2, s2) = randLetter s1
      (c3, s3) = randLetter s2
   in [c1, c2, c3]

generalA :: (a -> b) -> Gen a -> Gen b
generalA modifier rand =
  \seed ->
    let (a, seed') = rand seed
     in (modifier a, seed')

randEven :: Gen Integer 
randEven = generalA (* 2) rand

randOdd :: Gen Integer
randOdd = generalA (+ 1) rand

randTen :: Gen Integer
randTen = generalA (* 10) rand

randPair :: Gen (Char, Integer)
randPair seed = 
  let (char, seed') = randLetter seed
      (num, seed'') = rand seed'
   in ((char, num), seed'')

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair randA randB =
  \seed ->
    let (a, seed') = randA seed
        (b, seed'') = randB seed'
     in ((a, b), seed'')

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB combiner randA randB =
  \seed ->
    let (a, seed') = randA seed
        (b, seed'') = randB seed'
     in ((combiner a b), seed'')

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 randA randB = 
  generalB (,) randA randB

repRandom :: [Gen a] -> Gen [a]
repRandom []      = \seed -> ([], seed)
repRandom (r:rs)  = generalB (:) r (repRandom rs)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo rand transformer =
  \seed -> 
    let (a, seed') = rand seed
     in transformer a seed'

mkGen :: a -> Gen a
mkGen a = \seed -> (a, seed)


