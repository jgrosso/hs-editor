module Utils
  ( (>>=>)
  , clamp
  , replicateM'
  ) where


default (Int)


(>>=>) :: (a -> IO a) -> (a -> IO a) -> a -> IO a
partiallyApplied >>=> fullyApplied = (\initial -> return initial >>= partiallyApplied >>= fullyApplied)
infixl 1 >>=>


replicateM' :: (a -> IO a) -> Int -> a -> IO a
replicateM' x n initialState = foldl (>>=) (return initialState) (replicate n x)


clamp :: forall a. (Ord a) => a -> a -> (a -> a) -> a -> a
clamp min' max' f x' = if x < min'
                       then min'
                       else if x > max'
                            then max'
                            else x
  where x :: a
        x = f x'
