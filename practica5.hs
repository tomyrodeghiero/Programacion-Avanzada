-- Copyright (C) 2022 Tomás Rodeghiero

-- 1.
infListOf1 :: [Int]
infListOf1 = [x | x <- [1,1..]]

-- 2.
listNat :: Int -> [Int]
listNat n = [x | x <- [n..20]]

-- 3.
listFstNat :: Int -> [Int]
listFstNat n = [x | x <- [0..n]]

-- 4.
fst5Pos :: [Int]
fst5Pos = take 5 [x | x <- [1..]]

-- 5.
listOfSquares :: [Int] -> [Int]
listOfSquares [] = []
listOfSquares xs = map (^2) xs

-- 6.
divisorsNum :: Int -> [Int]
divisorsNum n = filter (\x -> n `mod` x == 0) [1..n]

-- 7.
isqrt :: Float -> Int
isqrt n = floor(sqrt n)

divsSqrt :: Int -> [Int]
divsSqrt n = filter (\x -> n `mod` x == 0) [2..isqrt(fromIntegral n)]

listPrimFromRand :: [Int] -> [Int]
listPrimFromRand [] = []
listPrimFromRand xs = filter (\x -> divsSqrt x == []) xs

-- 8.
sumOfSquaresList :: [Int] -> Int
sumOfSquaresList [] = 0
sumOfSquaresList xs = foldr (+) 0 (map (^2) xs)

-- 9.
listWithHisSucc :: [Int] -> [Int]
listWithHisSucc [] = []
listWithHisSucc xs = map (+1) xs

-- 10.
sumList :: [Int] -> Int
sumList [] = 0
sumList xs = foldr (+) 0 xs

-- 11.
fact' :: [Int] -> Int
fact' [] = 0
fact' xs = foldl (*) 1 xs

fact'' :: [Int] -> Int
fact'' [] = 0
fact'' xs = foldr (*) 1 xs

-- 12.
listByComprSucc :: [Int] -> [Int]
listByComprSucc [] = []
listByComprSucc xs = [x + 1 | x <- xs]

-- 13.
listByComprSquares :: [Int] -> [Int]
listByComprSquares [] = []
listByComprSquares xs = [x^2 | x <- xs]

-- 14.
pairsGreather10 :: [Int] -> [Int]
pairsGreather10 [] = []
pairsGreather10 xs = [x | x <- xs, x `mod` 2 == 0 && x > 10]

-- 15.
divs :: Int -> [Int]
divs 0 = []
divs n = [x | x <- [1..n], n `mod` x == 0]

-- 16.
primo :: Int -> Bool
primo n = divsSqrt n == []

listPrimFrom2ToN :: Int -> [Int]
listPrimFrom2ToN 0 = []
listPrimFrom2ToN n = [x | x <- [2..n], primo x]

-- 17.
cartesianProduct :: [Int] -> [Int] -> [(Int, Int)]
cartesianProduct xs [] = []
cartesianProduct [] ys = []
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

-- 18.
infListOfPairs :: [Int]
infListOfPairs = [x | x <- [0,2..]]

infListOfPairs' :: [Int]
infListOfPairs' = [x | x <- [0..], x `mod` 2 == 0]