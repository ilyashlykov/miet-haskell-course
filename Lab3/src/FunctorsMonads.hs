module FunctorsMonads where

-- определяем аналоги стандартных классов Functor и Monad,
-- чтобы нам не мешали их существующие экземпляры

-- документация на стандартные версии:
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor.html
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html
-- заметьте, что посмотрев в источники, можно взять реализацию оттуда;
-- постарайтесь этого не делать.

class Functor' f where
  (<$$>) :: (a -> b) -> f a -> f b

infixl 4 <$$>

class Functor' f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

infixl 4 <**>

-- Задание 1 -----------------------------------------

-- реализуйте join' через >>== и наоборот
class Applicative' m => Monad' m where
  (>>==) :: m a -> (a -> m b) -> m b
  (>>==) x f = join' (f <$$> x)

  join' :: m (m a) -> m a
  join' x = x >>== id

-- пример
instance Functor' Maybe where
  _ <$$> Nothing = Nothing
  f <$$> Just x = Just (f x)
instance Applicative' Maybe where
  pure' = Just
  Just f <**> Just x = Just (f x)
  _ <**> _ = Nothing
instance Monad' Maybe where
  -- достаточно было бы определить одну из функций >>== и join'
  Nothing >>== _ = Nothing
  Just x >>== f = f x
  join' Nothing = Nothing
  join' (Just x) = x

instance Functor' [] where
  f <$$> xs = map f xs
instance Applicative' [] where
  pure' x = [x]
  fs <**> xs = [f x | f <- fs, x <- xs]
instance Monad' [] where
  -- достаточно было бы определить одну из функций >>== и join'
  xs >>== f = [y | x <- xs, y <- f x]
  join' xss = [x | xs <- xss, x <- xs]

-- Задание 2 -----------------------------------------

-- Реализуйте функции, следуя типам.
-- liftA2' и seqA были на лекции, но:
-- 1. можно попробовать восстановить самостоятельно
-- 2. они могут быть использованы для других заданий

-- Добавьте тесты! Используйте не только те примеры, которые приведены здесь.
-- Попробуйте написать тесты _до_ реализации, чтобы проверить, правильных ли
-- результатов вы ожидаете. Можно также использовать тесты свойств.

liftA2' :: Applicative' f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f x y = f <$$> x <**> y

-- Выполняет все действия в списке и собирает их результаты в один список
seqA :: Applicative' f => [f a] -> f [a]
seqA = foldr (liftA2' (:)) (pure' [])

-- Применяет функцию, возвращающую действия, ко всем элементам списка, выполняет эти действия и собирает результаты в список
traverseA :: Applicative' f => (a -> f b) -> [a] -> f [b]
traverseA _ [] = pure' []
traverseA f (x:xs) = liftA2' (:) (f x) (traverseA f xs)

-- Фильтрует список, используя "предикат с эффектом".
filterA :: Applicative' f => (a -> f Bool) -> [a] -> f [a]
filterA _ [] = pure' []
filterA p (x:xs) = liftA2' (\flag rest -> if flag then x:rest else rest) (p x) (filterA p xs)

-- Композиция монадических функций
composeM :: Monad' m => (b -> m c) -> (a -> m b) -> (a -> m c)
composeM f g x = g x >>== f

-- Задание 3 -----------------------------------------

-- Реализуйте экземпляры классов типов

-- Добавьте тесты на поведение функций из задания 2 с этими экземплярами

-- Примеры для Either
instance Functor' (Either t) where
  _ <$$> Left x = Left x
  f <$$> Right x = Right (f x)

instance Applicative' (Either t) where
  pure' = Right
  Left x <**> _ = Left x
  Right f <**> rx = f <$$> rx

instance Monad' (Either t) where
  Left x >>== _ = Left x
  Right x >>== f = f x

-- Примеры для функций (->)
instance Functor' ((->) t) where
  f <$$> g = f . g

instance Applicative' ((->) t) where
  f <**> g = \x -> f x (g x)
  pure' x _ = x

instance Monad' ((->) t) where
  g >>== f = \x -> f (g x) x
