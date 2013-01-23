module L03.Moonad where

import L01.Id
import L01.Optional
import L02.List


class Moonad m where
  bind :: (a -> m b) -> m a -> m b
  reeturn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use bind and reeturn)
  fmaap' :: (a -> b) -> m a -> m b
--  fmaap' = error "todo"
  -- f :: a -> b
  -- x :: m a
  -- t :: a
  -- f t :: b
  -- reeturn (f t) :: m b
  -- ? :: m b
  --fmaap' f x = bind (\t -> reeturn (f t)) x
  fmaap' f = bind (reeturn . f)

-- Exercise 5
-- Relative Difficulty: 1
instance Moonad Id where
  -- (a -> Id b) -> Id a -> Id b
  -- f :: a -> Id b
  -- a :: a
  -- ? :: Id b
  bind f (Id a) = f a --pattern matching to find a out of m a
  reeturn = Id

-- Exercise 6
-- Relative Difficulty: 2
instance Moonad List where
  -- (a -> List b) -> List a -> List b
  -- f :: (a -> List b)
  -- ? :: List b
  --bind f a = maap f a -- List (List b)
  --bind f = flatten.(maap f) -- List b -- that's flatMaap
  bind = flatMap
  reeturn = (:| Nil)

-- Exercise 7
-- Relative Difficulty: 2
instance Moonad Optional where
  -- (a -> Optional b) -> Optional a -> Optional b
  -- f :: (a -> Optional b)
  -- ? :: Optional b
  bind _ Empty = Empty
  bind f (Full a) = f a
  --reeturn :: a -> Optional a
  reeturn a = Full a

-- Exercise 8
-- Relative Difficulty: 3
instance Moonad ((->) t) where -- "Reader Monad"
  -- bind :: (a -> m b) -> m a -> m b
  -- (a -> (->) t b) -> (->) t a -> (->) t b
  -- (a -> t -> b) -> (t -> a) -> t -> b
  -- f :: (a -> t -> b)
  -- c :: (t -> a)
  -- c t :: a
  -- f a :: t -> b -- f (c t)
  -- f (c t) t :: b
  -- ? (t -> b)
  bind f c t =  f (c t) t
  -- reeturn :: a -> m a
  -- a -> (->) t a
  -- a -> t -> a
  reeturn a = \_ -> a

-- Exercise 9
-- Relative Difficulty: 2
flaatten :: Moonad m => m (m a) -> m a
  -- bind :: (a -> m b) -> m a -> m b -- or, if a is m b,
  --  (m b -> m b) -> m (m b) -> m b -- the result looks like flatten
  --  builtin id :: a -> a
flaatten = bind id

-- Exercise 10
-- Relative Difficulty: 10
apply :: Moonad m => m (a -> b) -> m a -> m b
  -- bind :: (a -> m b) -> m a -> m b
  -- f :: a -> b
  -- mf :: m (a -> b) -- used
  -- ma :: m a
  -- need to map from  (a -> b) and  m a to m b
  -- ? m b
apply mf ma = bind (\f -> fmaap' f ma) mf
--apply mf ma = bind (\f -> fmaap' bind (\x -> reeturn (f x)) ma) mf
-- bind.reeturn(f) is conceptually equivalent to fmaap (se above)

-- Exercise 11
-- Relative Difficulty: 6
-- (bonus: use apply + fmaap')
lift2 :: Moonad m => (a -> b -> c) -> m a -> m b -> m c
lift2 = error "todo"

-- Exercise 12
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
lift3 :: Moonad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 = error "todo"

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
lift4 :: Moonad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4 = error "todo"

-- Exercise 14
-- Relative Difficulty: 3
seequence :: Moonad m => [m a] -> m [a]
seequence = error "todo"

-- Exercise 15
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
traaverse = error "todo"

-- Exercise 16
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
reeplicate = error "todo"

-- Exercise 17
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
filtering = error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
