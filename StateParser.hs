module Parser where
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.State.Class

newtype StateM m s a = StateM { runStateM :: s -> m (a, s) }
  deriving (Functor)

instance Applicative f => Applicative (StateM f s) where
    pure v = StateM $ \s -> pure (v, s)

instance Monad m => Monad (StateM m s) where
    return = pure
    (>>=) (StateM a) f = StateM $ \s -> do
        (a, s') <- a s
        r <- runStateM (f a) s
        return r

instance Alternative m => Alternative (StateM m a) where
    empty = StateM $ \s -> empty
    (<|>) (StateM a) (StateM b) = StateM $ \s -> (a s) <|> (b s)

instance Monad m => MonadState s (StateM m s) where
    state f = StateM $ \s -> do
        return (f s)

type Parser a = ReaderT Int (StateM [] String) a

item :: Parser Char
item = state (\(x:xs) -> (x, xs))

  {-
    s <- get
    let (r, ns) = case s of
          [] -> ([], [])
          (x:xs) -> ([x], xs)
    put ns
    case r of
      [] -> empty
      [r] -> return r
-}

-- The StateM monad combines the functionality of list monad and state
-- monad into one type. The bind operator of this handles the state
-- manually while also using the bind of the inner monad the
-- alternative operator uses the alternative operator of the inner
-- monad.
