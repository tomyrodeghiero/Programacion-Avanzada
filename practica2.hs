-- Copyright (C) 2022 Tomás Rodeghiero

-- 2.
{- hd -}
hd :: [a] -> a
hd (x:xs) = x

{- tl -}
tl :: [a] -> [a]
tl (x:xs) = xs

{- last -}
-- Versión 1
last' :: [a] -> a
last' (x:xs) = head(reverse(x:xs))

-- Versión 2
last'' :: [a] -> a
last''  [x] = x
last'' (x:xs) = last'' xs

{- init -}
init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x: init' xs

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

{- insertarAlFinal -}
insertarAlFinal :: a -> [a] -> [a]
insertarAlFinal y [] = [y]
insertarAlFinal y (x:xs) = x : insertarAlFinal y xs

-- 4.
-- Versión 1
abs' :: Int -> Int
abs' n
    | n < 0 = n * (-1)
    | otherwise = n

-- Versión 2
abs'' :: Int -> Int
abs'' n | n >= 0 = n
        | n  < 0 = -n

-- 5.
isqrt :: Float -> Int
isqrt n = floor(sqrt n)

divsSqrt :: Int -> [Int]
divsSqrt n = [x | x <- [2..isqrt(fromIntegral n)], n `mod` x == 0]

primo :: Int -> Bool
primo n = divsSqrt n == []

-- 6.
listPrim :: Int -> [Int]
listPrim n = [x | x <- [1..n-1], primo x]

-- 7.
cantElem :: [a] -> Int
cantElem [] = 0
cantElem (x:xs) = 1 + cantElem xs

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = (last' (x:xs)) : reverseList (tomar ((cantElem (x:xs)) - 1) (x:xs))

-- 8.
eqList :: (Eq a) => [a] -> [a] -> Bool
eqList [] [] = True
eqList [] _ = False
eqList _ [] = False
eqList (x:xs) (y:ys) = x == y && eqList xs ys

-- 9.
palindromo :: (Eq a) => [a] -> Bool
palindromo [] = True
palindromo xs = eqList xs (reverseList xs)

{- Anexo -}
edad :: (Int, Int, Int) -> (Int, Int, Int) -> Int
edad (a,b,c) (x,y,z) = (z - c) - 1

{- Clase -}

-- Los parentesís se colocan de izquierda a derecha por defecto
-- nombreFuncion :: Restricciones => Perfil
-- funcion :: (Ord a) => [a] -> a -> Bool
-- funcion :: (Eq a) => [a] -> a -> Bool
-- (:) elemento lista (prefija)
-- elemento : lista (sufija)