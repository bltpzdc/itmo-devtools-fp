module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = [f n] ++ finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = [x] ++ ff f (f x)

digits :: Int -> [Int]
digits n
    | n < 10    = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

allDigits :: [Int] -> [Int]
allDigits []     = []
allDigits (n:ns) = digits n ++ allDigits ns

count :: Int -> [Int] -> Int
count _ [] = 0
count x (x':xs)
    | x == x'   = 1 + count x xs
    | otherwise = count x xs

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq ns = traverse 0 0 ds
    where
        ds = allDigits ns

        traverse cur best [] = cur
        traverse cur best (d:ds') =
            let c = count d ds
            in if c > best
               then traverse d c ds'
               else traverse cur best ds'
    
{-
Or
mostFreq ns =
    head
    . maximumBy (comparing length)
    . group
    . sort
    . allDigits ???
-}

contains :: (Eq a) => [a] -> a -> Bool
contains [] _ = False
contains (v:vs) x
    | x == v = True
    | otherwise = contains vs x

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (v:vs) = if contains us v then us else us ++ [v]
    where us = uniq vs

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f =
    foldr (
        \x acc ->
            let k = f x
            in case break ((== k) . fst) acc of
                (before, []) ->
                    (k, [x]) : acc
                (before, (k', xs) : after) ->
                    (k', x : xs) : before ++ after
    )
    []
