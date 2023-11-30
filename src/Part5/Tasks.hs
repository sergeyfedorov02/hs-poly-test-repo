module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
-- На входе у нас функция F типа (b -> a -> b), далее начальное значение acc типа b и список xs типа [a]
myFoldl _ acc [] = acc
-- Рекурсивно применим нашу функцию F к текущему значению аккумулятора и головному элементу x,
-- результат которого становится новым значение Acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
-- Воспользуемся правой сверткой
  -- Передадим в свертку анонимную функцию, которая берет текущий элемент x и аккумулятор acc,
  -- Затем добавляет результат применения функции f к элементу x в начало аккумулятора
  -- Изначально в аккумуляторе пустой список (получается, что проходимся по всем элементам справа налево)
myMap f xs = myFoldr (\x acc -> f x : acc) [] xs

myConcatMap :: (a -> [b]) -> [a] -> [b]
-- (++) - функция объединения списков
myConcatMap f xs = myFoldr (\x acc -> f x ++ acc) [] xs

myConcat :: [[a]] -> [a]
myConcat xs = myFoldr (++) [] xs

myReverse :: [a] -> [a]
-- "\" - начало объявления анонимной функции
-- (:) - для добавления элемента в начало списка
-- acc сначала, так как мы добавляем в начало списка
myReverse xs = myFoldl (\acc x -> x : acc) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
-- Если условие подходит, до добавляем элемент в начало списка
myFilter p xs = myFoldr (\x acc -> if p x then x : acc else acc) [] xs

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
-- Начальное значение аккумулятора - это два пустых списка для истинных и ложных результатов применения F к списку XS
myPartition p xs = myFoldr f ([], []) xs
  -- Напишем логику обработки для каждого элемента списка Xs
  where
    f x (true, false) = if p x then (x : true, false) else (true, x : false)

