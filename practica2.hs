-- 2.
{- hd -}
hd :: [a] -> a
hd (x:xs) = x

{- tl -}
tl :: [a] -> [a]
tl (x:xs) = xs

{- last -}
-- Versión 1
last_ :: [a] -> a
last_ (x:xs) = head(reverse(x:xs))

-- Versión 2
last__ :: [a] -> a
last__  [x] = x
last__ (x:xs) = last__ xs

{- init -}
init_ :: [a] -> [a]
init_ [x] = []
init_ (x:xs) = x: init_ xs

-- 3.
{- concatenar -}
concatenar :: a -> [a] -> [a]
concatenar x xs = x : xs 

{- tomar -}
tomar :: Int -> [a] -> [a]
tomar 0 xs = []
tomar n (x:xs) = x : tomar(n-1) xs

{- tirar -}
tirar :: Int -> [a] -> [a]
tirar 0 xs = xs
tirar n (x:xs) = tirar (n-1) xs

{- concFinal -}
concFinal :: a -> [a] -> [a]
concFinal y [] = [y]
concFinal y (x:xs) = concFinal y xs

-- 4.
-- Versión 1
absoluto_ :: Int -> Int
absoluto_ n
    | n < 0 = n * (-1)
    | otherwise = n

-- Versión 2
absoluto__ :: Int -> Int
absoluto__ n | n >= 0 = n
             | n  < 0 = -n

-- 5.
edad :: (Int, Int, Int) -> (Int, Int, Int) -> Int
edad (a,b,c) (x,y,z) = (z - c) - 1

-- 6.
isqrt :: Float -> Int
isqrt n = floor(sqrt n)

divsSqrt :: Int -> [Int]
divsSqrt n = [x | x <- [2..isqrt(fromIntegral n)], n `mod` x == 0]

primo :: Int -> Bool
primo n = divsSqrt n == []

-- 7.
listPrim :: Int -> [Int]
listPrim n = [x | x <- [1..n-1], primo x]

-- 8.
cantElem :: [a] -> Int
cantElem [] = 0
cantElem (x:xs) = 1 + cantElem xs

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = (last_ (x:xs)) : reverseList (tomar ((cantElem (x:xs)) - 1) (x:xs))

eqList :: (Eq a) => [a] -> [a] -> Bool
eqList [] [] = True
eqList [] _ = False
eqList _ [] = False
eqList (x:xs) (y:ys) = x == y && eqList xs ys

palindromo :: (Eq a) => [a] -> Bool
palindromo [] = True
palindromo xs = eqList xs (reverseList xs)