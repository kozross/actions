{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Biased
  ( Bias (..),
    Biased (..),
  )
where

import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Group (Group (invert))
import Data.Kind (Type)
import Data.Semigroup (Endo (Endo))
import Data.Semigroup.Action (Action (TargetOf, act), action)
import Data.Wedge (Wedge (Here, Nowhere, There))

-- | @since 1.0
data Bias = LeftHand | RightHand
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

newtype
  Biased
    (b :: Bias)
    (f :: Type -> Type -> Type)
    (a :: Type)
    (c :: Type) = Biased
  { unBiased :: f a c
  }
  deriving
    ( -- | @since 1.0
      Eq
    )
    via (f a c)
  deriving
    ( -- | @since 1.0
      Functor
    )
    via (f a)
  deriving
    ( -- | @since 1.0
      Bifunctor
    )
    via f
  deriving stock
    ( -- @since 1.0
      Show
    )

-- | @since 1.0
instance
  (Semigroup a, Semigroup b) =>
  Semigroup (Biased 'LeftHand Either a b)
  where
  {-# INLINEABLE (<>) #-}
  Biased (Left x) <> Biased (Left y) = Biased . Left $ x <> y
  Biased (Right x) <> Biased (Right y) = Biased . Right $ x <> y
  x@(Biased (Left _)) <> _ = x
  _ <> y = y

-- | @since 1.0
instance
  (Semigroup a, Monoid b) =>
  Monoid (Biased 'LeftHand Either a b)
  where
  {-# INLINEABLE mempty #-}
  mempty = Biased . Right $ mempty

-- | @since 1.0
instance
  (Semigroup a, Semigroup b) =>
  Semigroup (Biased 'RightHand Either a b)
  where
  {-# INLINEABLE (<>) #-}
  Biased (Left x) <> Biased (Left y) = Biased . Left $ x <> y
  Biased (Right x) <> Biased (Right y) = Biased . Right $ x <> y
  x@(Biased (Right _)) <> _ = x
  _ <> y = y

-- | @since 1.0
instance
  (Monoid a, Semigroup b) =>
  Monoid (Biased 'RightHand Either a b)
  where
  {-# INLINEABLE mempty #-}
  mempty = Biased . Left $ mempty

-- | @since 1.0
instance
  (Monoid (Biased b Either a c), Group a, Group c) =>
  Group (Biased b Either a c)
  where
  {-# INLINEABLE invert #-}
  invert = bimap invert invert

-- | @since 1.0
instance
  (Semigroup (Biased b Either a c), Action a, Action c) =>
  Action (Biased b Either a c)
  where
  type TargetOf (Biased b Either a c) = Either (TargetOf a) (TargetOf c)
  {-# INLINEABLE act #-}
  act (Biased x) = Endo $ case bimap action action x of
    Left f -> first f
    Right g -> second g

-- | @since 1.0
instance
  (Semigroup a, Semigroup b) =>
  Semigroup (Biased 'LeftHand Wedge a b)
  where
  {-# INLINEABLE (<>) #-}
  Biased Nowhere <> x = x
  x <> Biased Nowhere = x
  Biased (Here x) <> Biased (Here y) = Biased . Here $ x <> y
  Biased (There x) <> Biased (There y) = Biased . There $ x <> y
  x@(Biased (Here _)) <> _ = x
  _ <> y = y

-- | @since 1.0
instance
  (Semigroup a, Semigroup b) =>
  Semigroup (Biased 'RightHand Wedge a b)
  where
  {-# INLINEABLE (<>) #-}
  Biased Nowhere <> x = x
  x <> Biased Nowhere = x
  Biased (Here x) <> Biased (Here y) = Biased . Here $ x <> y
  Biased (There x) <> Biased (There y) = Biased . There $ x <> y
  x@(Biased (There _)) <> _ = x
  _ <> y = y

-- | @since 1.0
instance (Semigroup (Biased b Wedge a c)) => Monoid (Biased b Wedge a c) where
  {-# INLINEABLE mempty #-}
  mempty = Biased Nowhere

-- | @since 1.0
instance
  (Group a, Group c, Semigroup (Biased b Wedge a c)) =>
  Group (Biased b Wedge a c)
  where
  {-# INLINEABLE invert #-}
  invert = bimap invert invert
