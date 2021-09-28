{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.Update
  ( -- * Update transformer
    UpdateT,

    -- * Obtain results
    runUpdateT,
    evalUpdateT,
    execUpdateT,
    traceUpdateT,
    simulateUpdateT,

    -- * Operations
    submit,
    submitM,
    apply,
    applyM,
    query,
  )
where

import Control.Applicative
  ( Alternative (empty, (<|>)),
    WrappedMonad (WrapMonad),
    unwrapMonad,
  )
import Control.Applicative.Cancellative (Cancellative (cancel))
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bifunctor (bimap, first)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Bind (Bind ((>>-)))
import Data.Functor.Bind.Trans (BindTrans (liftB))
import Data.Functor.Plus (Plus)
import Data.Group (Group (invert))
import Data.Kind (Type)
import Data.Semigroup.Action (Action (TargetOf), action)
import qualified Semigroupoids.Do as Bind

-- | @since 1.0
newtype
  UpdateT
    (w :: Type)
    (s :: Type)
    (m :: Type -> Type)
    (a :: Type) = UpdateT
  { -- | Perform the computation, but don't change the state, instead
    -- presenting only what changes /would/ have happened.
    --
    -- @since 1.0
    simulateUpdateT :: s -> m (w, a)
  }
  deriving stock
    ( -- | @since 1.0
      Functor
    )

-- | @since 1.0
instance
  (Bind m, Action w, s ~ TargetOf w) =>
  Apply (UpdateT w s m)
  where
  {-# INLINEABLE (<.>) #-}
  UpdateT fs <.> UpdateT xs = UpdateT $ \st -> Bind.do
    (w1, f) <- fs st
    let st2 = action w1 st
    bimap (w1 <>) f <$> xs st2

-- | @since 1.0
instance
  (Monad m, Action w, s ~ TargetOf w, Monoid w) =>
  Applicative (UpdateT w s m)
  where
  {-# INLINEABLE pure #-}
  pure x = UpdateT $ \_ -> pure (mempty, x)
  {-# INLINEABLE (<*>) #-}
  (<*>) = liftUpdate2 (<.>)

-- | @since 1.0
instance
  (Plus m, Action w, s ~ TargetOf w) =>
  Alt (UpdateT w s m)
  where
  {-# INLINEABLE (<!>) #-}
  UpdateT xs <!> UpdateT ys = UpdateT $ \st -> xs st <!> ys st

-- | @since 1.0
instance
  (MonadPlus m, Action w, s ~ TargetOf w, Monoid w) =>
  Alternative (UpdateT w s m)
  where
  {-# INLINEABLE empty #-}
  empty = UpdateT $ const empty
  {-# INLINEABLE (<|>) #-}
  (<|>) = liftUpdate2 (<!>)

-- | @since 1.0
instance
  (MonadPlus m, Cancellative m, Action w, s ~ TargetOf w, Group w) =>
  Cancellative (UpdateT w s m)
  where
  {-# INLINEABLE cancel #-}
  cancel (UpdateT f) = UpdateT $ \st -> first invert <$> cancel (f st)

-- | @since 1.0
instance
  (Bind m, Action w, s ~ TargetOf w) =>
  Bind (UpdateT w s m)
  where
  {-# INLINEABLE (>>-) #-}
  UpdateT f >>- g = UpdateT $ \st -> Bind.do
    (w1, x) <- f st
    let st2 = action w1 st
    let (UpdateT h) = g x
    first (w1 <>) <$> h st2

-- | @since 1.0
instance
  (Monad m, Action w, s ~ TargetOf w, Monoid w) =>
  Monad (UpdateT w s m)
  where
  {-# INLINEABLE (>>=) #-}
  (>>=) = liftBind (>>-)

-- | @since 1.0
instance (Monoid w) => MonadTrans (UpdateT w s) where
  {-# INLINEABLE lift #-}
  lift comp = UpdateT $ \_ -> (mempty,) <$> comp

-- | @since 1.0
instance (Monoid w) => BindTrans (UpdateT w s) where
  {-# INLINEABLE liftB #-}
  liftB comp = UpdateT $ \_ -> (mempty,) <$> comp

-- | @since 1.0
instance
  (MonadIO m, s ~ TargetOf w, Action w, Monoid w) =>
  MonadIO (UpdateT w s m)
  where
  {-# INLINEABLE liftIO #-}
  liftIO x = UpdateT $ \_ -> (mempty,) <$> liftIO x

-- | Perform the computation, modifying the state according to the action.
--
-- @since 1.0
runUpdateT ::
  forall (w :: Type) (m :: Type -> Type) (a :: Type).
  (Action w, Functor m) =>
  TargetOf w ->
  UpdateT w (TargetOf w) m a ->
  m (TargetOf w, a)
runUpdateT st (UpdateT f) =
  first (($ st) . action) <$> f st

-- | Perform the computation, discarding the state.
--
-- @since 1.0
evalUpdateT ::
  forall (w :: Type) (s :: Type) (m :: Type -> Type) (a :: Type).
  (Functor m) =>
  s ->
  UpdateT w s m a ->
  m a
evalUpdateT st (UpdateT f) = snd <$> f st

-- | Perform the computation, modifying the state according to the action, then
-- discard the result.
--
-- @since 1.0
execUpdateT ::
  forall (w :: Type) (m :: Type -> Type) (a :: Type).
  (Action w, Functor m) =>
  TargetOf w ->
  UpdateT w (TargetOf w) m a ->
  m (TargetOf w)
execUpdateT st (UpdateT f) =
  ($ st) . action . fst <$> f st

-- | Perform the computation, but don't modify the state, instead returning a
-- description of what would happen. Discard the result.
--
-- @since 1.0
traceUpdateT ::
  forall (w :: Type) (s :: Type) (m :: Type -> Type) (a :: Type).
  (Functor m) =>
  s ->
  UpdateT w s m a ->
  m w
traceUpdateT st (UpdateT f) = fst <$> f st

-- | Perform the action specified.
--
-- @since 1.0
submit ::
  forall (w :: Type) (s :: Type) (m :: Type -> Type).
  (Applicative m) =>
  w ->
  UpdateT w s m ()
submit x = UpdateT $ \_ -> pure (x, ())

-- | Perform the action, provided in the context of @m@.
--
-- @since 1.0
submitM ::
  forall (w :: Type) (s :: Type) (m :: Type -> Type).
  (Functor m) =>
  m w ->
  UpdateT w s m ()
submitM x = UpdateT $ \_ -> (,()) <$> x

-- | Perform the action specified, then yield the state that would result.
--
-- @since 1.0
apply ::
  forall (w :: Type) (m :: Type -> Type).
  (Action w, Applicative m) =>
  w ->
  UpdateT w (TargetOf w) m (TargetOf w)
apply x = UpdateT $ \st -> pure (x, action x st)

-- | Perform the action specified in the context of @m@, then yield the state
-- that would result.
--
-- @since 1.0
applyM ::
  forall (w :: Type) (m :: Type -> Type).
  (Action w, Functor m) =>
  m w ->
  UpdateT w (TargetOf w) m (TargetOf w)
applyM x = UpdateT $ \st -> (\y -> (y, action y st)) <$> x

-- | Short for @'apply' 'mempty'@.
--
-- @since 1.0
query ::
  forall (w :: Type) (m :: Type -> Type).
  (Action w, Monoid w, Applicative m) =>
  UpdateT w (TargetOf w) m (TargetOf w)
query = apply mempty

-- Helpers

liftUpdate :: UpdateT w s m a -> UpdateT w s (WrappedMonad m) a
liftUpdate (UpdateT f) = UpdateT $ WrapMonad . f

lowerUpdate :: UpdateT w s (WrappedMonad m) a -> UpdateT w s m a
lowerUpdate (UpdateT f) = UpdateT $ unwrapMonad . f

liftUpdate2 ::
  ( UpdateT w s (WrappedMonad m) a ->
    UpdateT w s (WrappedMonad m) b ->
    UpdateT w s (WrappedMonad m) c
  ) ->
  UpdateT w s m a ->
  UpdateT w s m b ->
  UpdateT w s m c
liftUpdate2 f x = lowerUpdate . f (liftUpdate x) . liftUpdate

liftBind ::
  ( UpdateT w s (WrappedMonad m) a ->
    (a -> UpdateT w s (WrappedMonad m) b) ->
    UpdateT w s (WrappedMonad m) b
  ) ->
  UpdateT w s m a ->
  (a -> UpdateT w s m b) ->
  UpdateT w s m b
liftBind f x g = lowerUpdate . f (liftUpdate x) $ (liftUpdate . g)
