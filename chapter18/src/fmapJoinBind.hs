import Control.Monad (join)

-- Remember:
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- join :: Monad m => m (m a) -> m a

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m
