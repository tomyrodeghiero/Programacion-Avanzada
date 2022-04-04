-- Copyright (C) 2022 Tomás Rodeghiero

-- 1.
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = x : (merge xs (y:ys))
                    | otherwise = y : (merge  (x:xs) ys) -- y <= x

-- 2.
minElem :: [Int] -> Int
minElem [] = 0
minElem [x] = x
minElem (x:(y:ys)) | x > y = minElem (y:ys)
                   | x <= y = minElem (x:ys)
                   
removeElem :: Int -> [Int] -> [Int]
removeElem n [] = []
removeElem n (x:xs) | n == x = xs
                    | otherwise = x : removeElem n xs

selSort :: [Int] -> [Int]
selSort [] = []
selSort [x] = [x]
selSort (x:(y:ys)) = minElemento : selSort (removeElem minElemento list)
                     where minElemento = minElem(x:y:ys)
                           list = (x:y:ys)

-- 3.
twoRaisedN :: Int -> Int -> Int
twoRaisedN n mult | n == 0 = 0
                  | n == 1 = 2
                  | otherwise = 2 * twoRaisedN (n-1) (mult+1)

twoRaisedN' :: Int -> Int
twoRaisedN' n | n == 0 = 0
              | n == 1 = 2
              | otherwise = 2 * twoRaisedN' (n-1)

-- 4.
natToBinaryRev :: Int -> [Int]
natToBinaryRev 0 = []
natToBinaryRev x | x `mod` 2 == 0 = 0 : natToBinaryRev (x `div` 2)
                 | otherwise = 1 : natToBinaryRev (x `div` 2)

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary xs = reverse(natToBinaryRev xs)

-- 5.
binaryToDecimal :: Int -> Int
binaryToDecimal 0 = 0
binaryToDecimal bit = 2 * binaryToDecimal (bit `div` 10) + (bit `mod` 10)

-- 6.
perfectSquare :: Int -> Bool
perfectSquare n | n == 2 = True
                | sum [x | x <- [1,3..n]] == n = True
                | otherwise = False

-- 7.
repeated :: a -> Int -> [a]
repeated z 0 = []
repeated z r | r == 1 = [z]
             | otherwise = z : repeated z (r-1)

-- 8.
nelem :: (Eq a) => [a] -> Int -> a
nelem (x:xs) n | n == 0 = x
               | otherwise = nelem xs (n-1)

{- Clase -}

{-
Intercala los elementos de dos listas en una
ejemplo: intercalar [1,3] [2,4] = [1,2,3,4]
-}
--intercalar :: [a] -> [a] -> [a]
--intercalar xs [] = xs
--intercalar [] ys = ys
--intercalar (x:xs) (y:ys) = [x,y] ++ (intercalar xs ys)

{-
Separa los elementos de una lista en una tupla.
El primer elemento de la tupla contiene todos los valores de la
lista original en posiciones pares
El segundo elemento de la tupla contiene todos los valores de la lista original en posiciones impares
ejemplo: separar [1,2,3] = ([1,3], [2])
-}
separar :: [a] -> ([a],[a])
separar [] = ([],[])
separar [x] = ([x],[])
separar (x:(y:ys)) = (x:restoI, y:restoD)
                 where (restoI, restoD) = separar ys

{-
Dada una lista ordenada en orden ascendente y un valor, utiliza
una búsqueda dicotómica para buscar el valor.
Retornar True si y solo si la lista contiene al valor
ejemplo: existe [1,2,3,4] 2 = True
-}
existe :: [Int] -> Int -> Bool
existe [] e = False
existe [x] e = x == e
existe xs e | e == primeroDer = True
            | e > primeroDer = existe parteDer e
            | otherwise = existe parteIzq e
     where longitud = length xs
           mitad = (longitud `div` 2) --div es la división entera
           parteIzq = take mitad xs
           parteDer = drop mitad xs
           primeroDer = (head parteDer)
{-
Determina si un valor (>0) es primo
-}     
esPrimo :: Int -> Bool
esPrimo n = divisores n == 2

{-
Devuelve la cantidad de divisores para un número n entre 1 y n
-}
divisores :: Int -> Int
divisores n = divisores' n n

{-
Devuelve la cantidad de divisores para un número n entre 1 y k
-}
divisores' :: Int -> Int -> Int
divisores' n 1 = 1
divisores' n k | (n `mod` k) == 0 = 1 + divisores' n (k-1)
               | otherwise = divisores' n (k-1)