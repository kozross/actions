{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Update
  ( MonadUpdate (..),
    query,
  )
where

import Control.Monad (void)
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.CPS as RWSC
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Select (SelectT)
import qualified Control.Monad.Trans.State.Lazy as StateL
import qualified Control.Monad.Trans.State.Strict as StateS
import Control.Monad.Trans.Update (UpdateT)
import qualified Control.Monad.Trans.Update as Update
import qualified Control.Monad.Trans.Writer.CPS as WriterC
import qualified Control.Monad.Trans.Writer.Lazy as WriterL
import qualified Control.Monad.Trans.Writer.Strict as WriterS
import Data.Kind (Type)
import Data.Semigroup.Action (Action (TargetOf))

-- | @since 1.0
class
  (Monad m, Action w) =>
  MonadUpdate (w :: Type) (m :: Type -> Type)
    | m -> w
  where
  -- | @since 1.0
  apply :: w -> m (TargetOf w)

  -- | @since 1.0
  submit :: w -> m ()
  submit x = void . apply $ x

-- | @since 1.0
instance
  (Monad m, Action w, Monoid w, s ~ TargetOf w) =>
  MonadUpdate w (UpdateT w s m)
  where
  {-# INLINEABLE apply #-}
  apply = Update.apply
  {-# INLINEABLE submit #-}
  submit = Update.submit

-- | @since 1.0
instance (MonadUpdate w m, Monoid w') => MonadUpdate w (AccumT w' m) where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance (MonadUpdate w m) => MonadUpdate w (ContT r m) where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance (MonadUpdate w m) => MonadUpdate w (ExceptT e m) where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance (MonadUpdate w m) => MonadUpdate w (MaybeT m) where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance (MonadUpdate w m) => MonadUpdate w (ReaderT r m) where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance
  (MonadUpdate w m, Monoid w') =>
  MonadUpdate w (RWSL.RWST r w' s m)
  where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance
  (MonadUpdate w m, Monoid w') =>
  MonadUpdate w (RWSS.RWST r w' s m)
  where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance
  (MonadUpdate w m) =>
  MonadUpdate w (RWSC.RWST r w' s m)
  where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance (MonadUpdate w m) => MonadUpdate w (SelectT r m) where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance
  (MonadUpdate w m) =>
  MonadUpdate w (StateL.StateT s m)
  where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance
  (MonadUpdate w m) =>
  MonadUpdate w (StateS.StateT s m)
  where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance
  (MonadUpdate w m, Monoid w') =>
  MonadUpdate w (WriterL.WriterT w' m)
  where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance
  (MonadUpdate w m, Monoid w') =>
  MonadUpdate w (WriterS.WriterT w' m)
  where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
instance
  (MonadUpdate w m) =>
  MonadUpdate w (WriterC.WriterT w' m)
  where
  {-# INLINEABLE apply #-}
  apply = lift . apply
  {-# INLINEABLE submit #-}
  submit = lift . submit

-- | @since 1.0
query :: (MonadUpdate w m, Monoid w) => m (TargetOf w)
query = apply mempty
