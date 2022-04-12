-- Copyright (C) 2022 Tomás Rodeghiero

-- 1.
data Nat = Zero | Succ Nat deriving Show

-- 2.
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + (natToInt n)

-- 3.
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n-1))

-- 4.
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero Zero = Zero
sumaNat Zero (Succ n) = Succ n
sumaNat (Succ n) Zero  = Succ n
sumaNat (Succ n1) (Succ n2) = sumaNat n1 (Succ (Succ n2))

sumaNat' :: Nat -> Nat -> Nat
sumaNat' n1 n2 = intToNat ((natToInt n1) + (natToInt n2))

-- 5.
{- Un árbol binario es un conjunto finito de
nodos que puede estar vacío o consistir en un nodo raíz y
dos árboles binarios disjuntos, llamados subárbol izquierdo (o rama izquierda)
y subárbol derecho (o rama derecha). -}

-- 6.
data Arbol = Nil | Bin Arbol Arbol deriving Show

size :: Arbol -> Int
size Nil = 0
size (Bin rI rD) = 1 + size rI + size rD

-- 7.
-- La rama de un árbol es la longitud de la rama más larga del Arbol, en la cual se cuentan por cantidad de nodos
height :: Arbol -> Int
height Nil = 0
height (Bin rI rD) = 1 + max (height rI) (height rD)


{-
{- Nil: Caso base, Bin: Caso recursivo -}

-- Esquema de recursión estructural para el tipo de datos Arbol
-- f :: Arbol -> ...
-- f Nil = ...
-- f (Bin i d) = ...

esNil :: Arbol -> Bool
esNil Nil = True
esNil (Bin i d) = False

-- Precondición: el Arbol no debe ser Nil
hijoIzq :: Arbol -> Arbol
hijoIzq (Bin i d) = i

-- Precondición: el Arbol no debe ser Nil
hijoDer :: Arbol -> Arbol
hijoDer (Bin i d) = d 

cantHojas :: Arbol -> Int
cantHojas Nil = 0
cantHojas (Bin i d) = if esNil i && esNil d then 1 else cantHojas i + cantHojas d


-}

{-
## Tipos
​
Un tipo se define como
​
data nombre = constructores
​
-}
--data Variable = Variable Int


{-data Variable = Variable {valor::Int} deriving (Show, Eq)
​
data ListaEntera = Vacia | Cons Int ListaEntera deriving (Show, Eq)
​
data Booleano = Verdadero | Falso deriving (Show, Eq)
​
esVerdadero :: Booleano -> Bool
esVerdadero Verdadero = True
esVerdadero Falso = False
​
--data VariableMultiple = VariableM Int Int Int
data VariableMultiple = VariableM {valor1::Int, valor2::Int, valor3::Int}
​
variablePositiva :: Variable -> Bool
variablePositiva (Variable n) = n >= 0
​
foo :: [a] -> a
foo (x:xs) = x
​
data Quizas a = Nada | Justo {val::a} deriving (Show, Eq)
​
elementoEn :: [Int] -> Int -> Quizas Int
elementoEn [] _ = Nada
elementoEn (x:_) 0 = Justo x
elementoEn (x:xs) n | n < 0 = Nada
                    | otherwise = elementoEn xs (n - 1)
​
​
data AB a = Vacio | AB a (AB a) (AB a) deriving (Show, Eq)
​
antecesor :: (Eq e) => e -> AB e -> Quizas e
antecesor _ Vacio = Nada
antecesor elem ab = antecesor' elem ab Nada
​
antecesor' :: (Eq e) => e -> AB e -> Quizas e -> Quizas e
antecesor' _ Vacio _ = Nada
antecesor' elem (AB raiz rI rD) ant | elem == raiz = ant
                                    | antI /= Nada = antI
                                    | antD /= Nada = antD
                                    | otherwise = Nada
                                    where antI = antecesor' elem rI (Justo raiz)
                                          antD = antecesor' elem rD (Justo raiz) -}