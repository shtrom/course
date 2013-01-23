module L03.Fuunctor where

import L01.Id
import L01.Optional
import L01.Validation
import L02.List

class Fuunctor f where
  fmaap :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fuunctor Id where
  -- fmaap :: (a -> b) -> Id a -> Id b
  -- data Id a -> Id a
  -- f :: a -> b
  -- a :: a
  -- f a :: b
  -- Id (f a) :: Id b
  -- ? :: Id b
  fmaap f (Id a) = Id (f a)

-- Exercise 2
-- Relative Difficulty: 2
instance Fuunctor List where
  -- fmaap :: (a -> b) -> List a -> List b
  fmaap =
    maap

-- Exercise 3
-- Relative Difficulty: 2
instance Fuunctor Optional where
  -- fmaap :: (a -> b) -> Optional a -> Optional b
  -- data Optional a :: Full a | Empty
  -- f :: a -> b
  -- a :: a
  -- f a :: b
  -- Optional (f a) :: Optional b
  fmaap =
    mapOptional

-- Exercise 4
-- Relative Difficulty: 3
instance Fuunctor ((->) t) where
  -- fmaap :: (a -> b) -> ((->) t) a -> ((->) t) b -- '->' is right-associative
  -- fmaap :: (a -> b) -> (t -> a) -> (t -> b) -- this is function composition
  fmaap =
    (.)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fuunctor [] where
  fmaap = fmap
instance Fuunctor IO where
  fmaap = fmap
