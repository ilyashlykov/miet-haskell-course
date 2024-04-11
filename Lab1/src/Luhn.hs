module Luhn where

-- Проверка корректности номера банковской карты алгоритмом Луна https://ru.wikipedia.org/wiki/Алгоритм_Луна.
-- Алгоритм:
-- 1. Все цифры, стоящие на чётных местах (считая с конца), удваиваются. Если при этом получается число, большее 9, то из него вычитается 9. Цифры, стояшие на нечётных местах, не изменяются.
-- То есть: последняя цифра не меняется; предпоследнее удваивается; 3-е с конца (предпредпоследнее) не меняется; 4-е с конца удваивается и т.д.
-- 2. Все полученные числа складываются.
-- 3. Если полученная сумма кратна 10, то исходный список корректен.

-- Не пытайтесь собрать всё в одну функцию, используйте вспомогательные.
-- Например: разбить число на цифры (возможно, сразу в обратном порядке).
-- Не забудьте добавить тесты, в том числе для вспомогательных функций!

-- Функция для разбиения числа на цифры в обратном порядке
digitsRev :: Int -> [Int]
digitsRev n
  | n < 10    = [n]
  | otherwise = n `mod` 10 : digitsRev (n `div` 10)

-- Функция для удвоения и коррекции цифр на четных позициях
doubleEven :: [Int] -> [Int]
doubleEven []       = []
doubleEven [x]      = [x]
doubleEven (x:y:zs) = x : (if y * 2 > 9 then y * 2 - 9 else y * 2) : doubleEven zs

-- Функция для суммирования цифр
sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs

-- Функция для проверки корректности номера по алгоритму Луна
isLuhnValid :: Int -> Bool
isLuhnValid n = sumDigits (doubleEven (digitsRev n)) `mod` 10 == 0
