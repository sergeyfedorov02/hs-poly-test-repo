{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    zero :: Int -> Int -> mx
    eye ::  Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    zero _ _ = 0
    eye _ = 1

instance Matrix [[Int]] where
    -- Создаем матрицу нулей размера w x h
    -- replicate - создание списка определенной длины из повторяющихся элементов
    zero w h = replicate h (replicate w 0)
    -- Создаем единичную матрицу размера w x w
    eye w = [ [if i == j then 1 else 0
            | j <- [0..w-1]]
            | i <- [0..w-1] ]

instance Matrix (SparseMatrix Int) where
    -- Создаем пустую разреженную матрицу размера w x h
    zero w h = SparseMatrix w h Data.Map.empty
    -- Создаем единичную разреженную матрицу размера
    eye w = SparseMatrix w w $ Data.Map.fromList [((i, i), 1) | i <- [0..w-1]]

-- Реализуйте следующие функции
-- Единичная матрица
--eye :: Matrix m => Int -> m
--eye w = notImplementedYet
-- Матрица, заполненная нулями
--zero :: Matrix m => Int -> Int -> m
--zero w h = notImplementedYet
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = notImplementedYet
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
