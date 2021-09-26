{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
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

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bifunctor (bimap, first)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Functor.Bind (Bind ((>>-)))
import qualified Data.Functor.Bind.Syntax as Bind
import Data.Functor.Bind.Trans (BindTrans (liftB))
import Data.Functor.Plus (Plus)
import Data.Kind (Type)
import Data.Semigroup (appEndo)
import Data.Semigroup.Action (Action (TargetOf, act))

-- | @since 1.0
newtype
  UpdateT
    (w :: Type)
    (s :: Type)
    (m :: Type -> Type)
    (a :: Type) = UpdateT
  { -- | @since 1.0
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
    let st2 = appEndo (act w1) st
    bimap (w1 <>) f <$> xs st2

-- | @since 1.0
instance
  (Monad m, Action w, s ~ TargetOf w, Monoid w) =>
  Applicative (UpdateT w s m)
  where
  {-# INLINEABLE pure #-}
  pure x = UpdateT $ \_ -> pure (mempty, x)
  {-# INLINEABLE (<*>) #-}
  UpdateT fs <*> UpdateT xs = UpdateT $ \st -> do
    (w1, f) <- fs st
    let st2 = appEndo (act w1) st
    (w2, x) <- xs st2
    pure (w1 <> w2, f x)

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
  UpdateT xs <|> UpdateT ys = UpdateT $ \st -> xs st <|> ys st

-- | @since 1.0
instance
  (Bind m, Action w, s ~ TargetOf w) =>
  Bind (UpdateT w s m)
  where
  {-# INLINEABLE (>>-) #-}
  UpdateT f >>- g = UpdateT $ \st -> Bind.do
    (w1, x) <- f st
    let st2 = appEndo (act w1) st
    let (UpdateT h) = g x
    first (w1 <>) <$> h st2

-- | @since 1.0
instance
  (Monad m, Action w, s ~ TargetOf w, Monoid w) =>
  Monad (UpdateT w s m)
  where
  {-# INLINEABLE (>>=) #-}
  UpdateT f >>= g = UpdateT $ \st -> do
    (w1, x) <- f st
    let st2 = appEndo (act w1) st
    let (UpdateT h) = g x
    (w2, y) <- h st2
    pure (w1 <> w2, y)

-- | @since 1.0
instance (Monoid w) => MonadTrans (UpdateT w s) where
  {-# INLINEABLE lift #-}
  lift comp = UpdateT $ \_ -> (mempty,) <$> comp

-- | @since 1.0
instance (Monoid w) => BindTrans (UpdateT w s) where
  {-# INLINEABLE liftB #-}
  liftB comp = UpdateT $ \_ -> (mempty,) <$> comp

-- | @since 1.0
runUpdateT ::
  forall (w :: Type) (m :: Type -> Type) (a :: Type).
  (Action w, Functor m) =>
  TargetOf w ->
  UpdateT w (TargetOf w) m a ->
  m (TargetOf w, a)
runUpdateT st (UpdateT f) =
  first (($ st) . appEndo . act) <$> f st

-- | @since 1.0
evalUpdateT ::
  forall (w :: Type) (s :: Type) (m :: Type -> Type) (a :: Type).
  (Functor m) =>
  s ->
  UpdateT w s m a ->
  m a
evalUpdateT st (UpdateT f) = snd <$> f st

-- | @since 1.0
execUpdateT ::
  forall (w :: Type) (m :: Type -> Type) (a :: Type).
  (Action w, Functor m) =>
  TargetOf w ->
  UpdateT w (TargetOf w) m a ->
  m (TargetOf w)
execUpdateT st (UpdateT f) =
  ($ st) . appEndo . act . fst <$> f st

-- | @since 1.0
traceUpdateT ::
  forall (w :: Type) (s :: Type) (m :: Type -> Type) (a :: Type).
  (Functor m) =>
  s ->
  UpdateT w s m a ->
  m w
traceUpdateT st (UpdateT f) = fst <$> f st

-- | @since 1.0
submit ::
  forall (w :: Type) (s :: Type) (m :: Type -> Type).
  (Applicative m) =>
  w ->
  UpdateT w s m ()
submit x = UpdateT $ \_ -> pure (x, ())

-- | @since 1.0
submitM ::
  forall (w :: Type) (s :: Type) (m :: Type -> Type).
  (Functor m) =>
  m w ->
  UpdateT w s m ()
submitM x = UpdateT $ \_ -> (,()) <$> x

-- | @since 1.0
apply ::
  forall (w :: Type) (m :: Type -> Type).
  (Action w, Applicative m) =>
  w ->
  UpdateT w (TargetOf w) m (TargetOf w)
apply x = UpdateT $ \st -> pure (x, appEndo (act x) st)

-- | @since 1.0
applyM ::
  forall (w :: Type) (m :: Type -> Type).
  (Action w, Functor m) =>
  m w ->
  UpdateT w (TargetOf w) m (TargetOf w)
applyM x = UpdateT $ \st -> (\y -> (y, appEndo (act y) st)) <$> x

-- | @since 1.0
query ::
  forall (w :: Type) (m :: Type -> Type).
  (Action w, Monoid w, Applicative m) =>
  UpdateT w (TargetOf w) m (TargetOf w)
query = apply mempty
