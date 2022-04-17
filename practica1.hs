-- Copyright (C) 2022 Tomás Rodeghiero

-- 1.
{- Si bien para comprabar si (2^29)/(2^9) y 2^20 son iguales se lo ha hecho en la Consola, 
a modo de anexo se adjunta una función para evaluar si dos numeros son iguales -}
eqExpressions :: Int -> Int -> Bool
eqExpressions a b = a == b

-- 2.
sndElemString :: String -> Char
sndElemString str = head(tail(str))

-- 3.
fstElemStrRev :: String -> Char
fstElemStrRev str = head(reverse(str))

-- 4
evenNum :: [Int] -> Bool
evenNum num = head(reverse(num)) `mod` 2 == 0
-- numPar num = if head(reverse(num)) `mod` 2 == 0 then True else False

-- 5.
numMult3 :: [Int] -> Bool
numMult3 num = sum(num) `mod` 3 == 0

-- 6.
numMult6 :: [Int] -> Bool
numMult6 num = sum(reverse(num)) `mod` 6 == 0

-- 7.
{-  Esos números son 1, 2 y 3, ya que
    1 + 2 + 3 = 1 * 2 * 3
        6     =     6
-}
sumEqToProd :: [Int] -> Bool
sumEqToProd num = sum(num) == product(num)

-- 8.
palindrome :: String -> Bool
palindrome cadena = cadena == reverse(cadena)

-- 9.
{- La evaluación de la expresión {(head.(drop 3)) "0123456"} arrojá como resultado el valor '3', 
   el cual es de tipo caracter. La función sobre listas denominada 'tail' se podría implementar de esta manera.
-}