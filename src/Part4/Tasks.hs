module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = create REmpty
    where
      create acc [] = acc
      create acc (x:xs) = create (acc :< x) xs

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show x = "[" ++ shows x ++ "]"
      where
        shows REmpty = ""
        shows (REmpty :< x) = show x
        shows (xs :< x) = shows xs ++ "," ++ show x

instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) (xs :< x) (ys :< y) = (x == y) && (xs == ys)
    (==) _ _ = False
    (/=) x y = not (x == y)

instance Semigroup (ReverseList a) where
    xs <> REmpty = xs
    REmpty <> ys = ys
    xs <> (ys :< y) = (xs <> ys) :< y

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (xs :< x) = fmap f xs :< f x

instance Applicative ReverseList where
    pure x = REmpty :< x
    REmpty <*> _ = REmpty
    _ <*> REmpty = REmpty
    (fs :< f) <*> xs = (fs <*> xs) <> (fmap f xs)

instance Monad ReverseList where
    return = pure
    REmpty >>= _ = REmpty
    (xs :< x) >>= f = (xs >>= f) <> (f x)
