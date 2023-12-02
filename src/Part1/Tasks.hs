module Part1.Tasks where

import Util(notImplementedYet)

-- Функция унификации (унитарного преобразования), чтобы сделать область [-pi, pi)
unify :: (Floating a, RealFrac a) => a -> a
unify x = x - 2 * pi * (fromIntegral . round $ ( x / (2 * pi)))

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = res
    where
      newX = unify x
      -- Вычислим сумму значений, после удаления из них элементов типа NaN (не число)
      -- take 301 - количество N, которые будем рассматривать
      -- product - вычисление произведения всех элементов в списке ([2, 3, 4] -> 2 * 3 * 4 -> 24)
      res = sum . filter (not . isNaN) $ take 301
        [(((-1) ** n) * (newX ** (2 * n + 1))) / product [1..(2 * n + 1)]
        | n <- [0..]]

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = res
    where
      newX = unify x
      res = sum $ take 301
        [(((-1) ** n) * (newX ** (2 * n))) / product [1..(2 * n)]
        | n <- [0..]]

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD first second
  | second == 0 = abs first
  | otherwise = myGCD second (first `mod` second)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | year < 0 || month < 1 || month > 12 || day < 1 || day > 31 = False
  | month == 2 && isLeapYear = day <= 29
  | otherwise = day <= daysInMonth
  where
    -- Задаем переменные
    isLeapYear = (year `mod` 100 /= 0 && year `mod` 4 == 0) || (year `mod` 400 == 0)
    -- !! позволяет извлечь значение из списка по заданному индексу
    -- fromIntegral приводит значение (month - 1) к такому, чтобы его можно было использовать в качестве индекса
    daysInMonth = [31,28,31,30,31,30,31,31,30,31,30,31] !! fromIntegral (month - 1)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow number power
  | power == 0 = 1
  | power < 0 = 0
  | otherwise = number * myPow number (power - 1)

-- является ли данное число простым?
isPrime x
  | x < 2 = False
  | x == 2 = True
  | x `mod` 2 == 0 = False
  -- Иначе проверим, что все значения из списка удовлетворяют предикату (лямбда-функции) x `mod` m /= 0
  | otherwise = all (\m -> x `mod` m /= 0) [3,5..mySqrt (fromIntegral x)]
  where
    -- Определим корень из числа x и округлим его в меньшую сторону (композиция функций)
    mySqrt = floor . sqrt

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points =
  -- Разобьем входной список из координат на два списка из отдельно взятых координат X и Y
  -- Также n - количество кортежей во входном списке (число точек)
  let (x, y) = unzip points
      n = length points
  in 0.5 * abs
  -- По формуле Гаусса требуется:
  -- 1) сложить перемножения X текущей точки с Y следующей
  -- 2) вычесть пермножения Y текущей точки и X следующей
  -- 3) Взять модуль от всех этих значений и умножить на 0.5
  (last x * head y - last y * head x
     + sum [ x !! i * y !! (i + 1) - y !! i * x !! (i + 1) | i <- [0..n - 2] ])

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | g > (k2 + k1) = -1
  | g2 < (k2^2 + k1^2) = 1
  | g2 == (k2^2 + k1^2) = 2
  | otherwise = 0
  where
    g = maximum [a, b, c]
    k1 = minimum [a, b, c]
    k2 = a + b + c - g - k1
    g2 = g^2
