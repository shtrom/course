data MaybeTransformer m a =
  MaybeTransformer (m (Maybe a))

-- type-matching extractor to peel the MaybeTransformer constructor off
extractMT :: MaybeTransformer m x -> m (Maybe x)
extractMT (MaybeTransformer x) = x

-- => notation creates a context assuming it left-hand side exists
instance Functor m => Functor (MaybeTransformer m) where
  -- (a -> b) -> MaybeTransformer m a -> MaybeTransformer m b
  fmap f (MaybeTransformer x) = -- pattern-math MaybeTransformer out
    -- x :: m (Maybe a)
    MaybeTransformer (fmap (\ maya -> fmap f maya) x) -- maya :: Maybe a
    -- or: MaybeTransformer (fmap (fmap f) x)

-- fmap (+10) (MaybeTransformer [Just 8, Just 9])

instance Monad m => Monad (MaybeTransformer m) where
  -- a -> MaybeTransformer m a
  return a =
    -- ? :: m (Maybe a)
    MaybeTransformer (return (Just a))

  -- MaybeTransformer m a -> (a -> MaybeTransformer m b) -> MaybeTransformer m b
  -- x :: m (Maybe a)
  -- f :: a -> MaybeTransformer m b
  MaybeTransformer x >>= f =
    -- ? :: MaybeTransformer m b
    MaybeTransformer (x >>= \maya -> -- maya :: Maybe a
      case maya of
       Just a -> extractMT (f a) -- f a :: MaybeTransformer m b
       Nothing -> return Nothing) -- m Maybe b
