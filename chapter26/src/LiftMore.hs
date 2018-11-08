module LiftMore where

-- import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy (StateT(..))

-- 2

-- ugh, there's already a MonadTrans instance for StateT,
-- so we have to wrap it in a newtype.
newtype ST s m a = ST (StateT s m a)

instance MonadTrans (ST s) where
  lift ma = ST $
    (StateT $ \s -> do
      a <- ma
      return (a, s)
    )