-- Copyright (C) 2022 Tomás Rodeghiero

-- 1.
{- Si bien para comprabar si (2^29)/(2^9) y 2^20 son iguales se lo ha hecho en la Consola, 
a modo de anexo se adjunta una función para evaluar si dos numeros son iguales -}
expIguales :: Int -> Int -> Bool
expIguales a b = a == b

-- 2.
segElemCad :: String -> Char
segElemCad cadena = head(tail(cadena))

-- 3.
primElemCadRev :: String -> Char
primElemCadRev cadena = head(tail(cadena))

-- 4
numPar :: [Int] -> Bool
numPar num = head(tail(num)) `mod` 2 == 0
-- numPar num = if head(tail(num)) `mod` 2 == 0 then True else False

-- 5.
numMultiploDe3 :: [Int] -> Bool
numMultiploDe3 num = sum(num) `mod` 3 == 0

-- 6.
numMultiploDe6 :: [Int] -> Bool
numMultiploDe6 num = sum(tail(num)) `mod` 6 == 0

-- 7.
{-  Esos números son 1, 2 y 3, ya que
    1 + 2 + 3 = 1 * 2 * 3
        6     =     6
-}
sumIgualAProd :: [Int] -> Bool
sumIgualAProd num = sum(num) == product(num)

-- 8.
palindromo :: String -> Bool
palindromo cadena = cadena == reverse(cadena)

-- 9.
{- La evaluación de la expresión {(head.(drop 3)) "0123456"} arrojá como resultado el valor '3', 
   el cual es de tipo caracter. La función sobre listas denominada 'tail' se podría implementar de esta manera.
-}