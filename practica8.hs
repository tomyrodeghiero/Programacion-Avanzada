-- Copyright (C) 2022 Tomás Rodeghiero

-- 1.
nand :: Bool -> Bool -> Bool
nand a b | (a == b) == True = False
         | otherwise = True

-- 2.
maj :: Bool -> Bool -> Bool -> Bool
maj a b c | (a && b) == True || (a && c) == True || (b && c) == True = True
          | otherwise = False

-- 3.
-- forAll i : 0 <= i < #xs : p xs.i
even' :: Int -> Bool
even' x = x `mod` 2 == 0

forAll :: [a] -> (a -> Bool) -> Bool
forAll [] _ = False
forAll elements predicate = and [predicate x | x <- elements]

exists :: [a] -> (a -> Bool) -> Bool
exists [] _ = False
exists elements predicate = or [predicate x | x <- elements]

-- 4.
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' xs = sum [x | x <- xs]

producer :: (Num a) => [a] -> a
producer [] = 0
producer xs = product [x | x <- xs]

length' :: (Num a) => [a] -> a
length' [] = 0
length' xs = sum [1 | _ <- xs]