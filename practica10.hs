-- Copyright (C) 2022 Tomás Rodeghiero

-- 3.
{- · f xs dice si todos los elementos son iguales. -}
allElemEq :: Eq a => [a] -> Bool
allElemEq [] = True
allElemEq (x:xs) = g xs x
                where
                    g :: Eq a => [a]  -> a -> Bool
                    g [] z = True
                    g (x:xs) z = z == x && g xs z

ordCrec :: [Int] -> Bool
ordCrec [] = True
ordCrec [x] = True
ordCrec (x:xs) = x < (xs !! 0) && (ordCrec xs)

m :: [Int] -> Int
m [] = maxBound::Int
m (x:xs) = x `min` m xs

cantPi :: [Int] -> (Int, Int)
cantPi [] = (0, 0)
cantPi (x:xs) | esPar x = (1 + a, b)
           | not(esPar x) = (a, 1 + b)
          where (a, b) = cantPi xs

esPar :: Int -> Bool
esPar n = n `mod` 2 == 0