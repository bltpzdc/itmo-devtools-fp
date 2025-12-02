module Part1.Tasks where

import Util(notImplementedYet)

checkTrigArg :: Double -> Double
checkTrigArg x = if x' > pi then x' - 2 * pi else x'
    where x' = x - 2 * pi * fromIntegral (floor (x / (2 * pi)))

taylor :: Double -> Double -> Double -> Double -> Double -> Double
taylor x term n sum eps
    | abs term <= eps = sum
    | otherwise = taylor x next (n + 2) (sum + term) eps
  where next = (-term) * x * x / ((n + 1) * (n + 2)) 

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = taylor x' x' 1 0 1e-6
    where x' = checkTrigArg x

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = taylor x' 1 0 0 1e-6
    where x' = checkTrigArg x

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD lhs 0 = abs lhs
myGCD lhs rhs = myGCD rhs (lhs `mod` rhs)

isLeap :: Integer -> Bool
isLeap year
    | mod year 400 == 0 = True
    | mod year 100 == 0 = False
    | mod year 4 == 0   = True
    | otherwise         = False

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year 
    | day <= 0 || month <= 0 || month > 12 || year < 0  = False
    | elem month [01, 03, 05, 07, 08, 10, 12]           = day <= 31
    | elem month [04, 06, 09, 11]                       = day <= 30
    | month == 02                                       = if isLeap year then day <= 29 else day <= 28
    | otherwise                                         = False

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow x n
    | even n    = myPow (x * x) (n `div` 2)
    | otherwise = x * myPow x (n - 1)

isPrime' :: Integer -> Integer -> Bool
isPrime' n d
    | d * d > n      = True
    | n `mod` d == 0 = False
    | otherwise      = isPrime' n (d + 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x
    | x <= 1 = False
    | otherwise = isPrime' x 2

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea points =
    let shifted = tail points ++ [head points]
        cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2
        s = sum (zipWith cross points shifted)
    in abs s / 2

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | not (isTriangle a b c) = -1
    | a'2 > b'2 + c'2 = 0
    | a'2 == b'2 + c'2 = 2
    | otherwise = 1
    where
        (a', b', c')
            | a > b && a > c = (a, b, c)
            | b > c = (b, a, c)
            | otherwise = (c, a, b)
        a'2 = a' * a'
        b'2 = b' * b'
        c'2 = c' * c'

        isTriangle x y z = 
            x > 0 && y > 0 && z > 0
         && x + y > z
         && y + z > x
         && x + z > y
