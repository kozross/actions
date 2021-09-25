{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Handedness
  ( Lefty (..),
    Righty (..),
  )
where

import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Group (Group (invert))
import Data.Kind (Type)
import Data.Semigroup (Endo (Endo))
import Data.Semigroup.Action (Action (TargetOf, act))

-- | @since 1.0
newtype Lefty (a :: Type) (b :: Type) = Lefty
  { -- | @since 1.0
    getLefty :: Either a b
  }
  deriving
    ( -- | @since 1.0
      Eq
    )
    via (Either a b)
  deriving
    ( -- | @since 1.0
      Functor
    )
    via (Either a)
  deriving
    ( -- | @since 1.0
      Bifunctor
    )
    via Either
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance (Semigroup a, Semigroup b) => Semigroup (Lefty a b) where
  {-# INLINEABLE (<>) #-}
  Lefty x <> Lefty y = Lefty $ case (x, y) of
    (Left x', Left y') -> Left $ x' <> y'
    (Left _, Right _) -> x
    (Right _, Left _) -> y
    (Right x', Right y') -> Right $ x' <> y'

-- | @since 1.0
instance (Semigroup a, Monoid b) => Monoid (Lefty a b) where
  {-# INLINEABLE mempty #-}
  mempty = Lefty . Right $ mempty

-- | @since 1.0
instance (Group a, Group b) => Group (Lefty a b) where
  {-# INLINEABLE invert #-}
  invert = bimap invert invert

-- | @since 1.0
instance (Action a, Action b) => Action (Lefty a b) where
  type TargetOf (Lefty a b) = Either (TargetOf a) (TargetOf b)
  {-# INLINEABLE act #-}
  act (Lefty x) = Endo $ case bimap act act x of
    Left (Endo f) -> first f
    Right (Endo g) -> second g

-- | @since 1.0
newtype Righty (a :: Type) (b :: Type) = Righty
  { -- | @since 1.0
    getRighty :: Either a b
  }
  deriving
    ( -- | @since 1.0
      Eq
    )
    via (Either a b)
  deriving
    ( -- | @since 1.0
      Functor
    )
    via (Either a)
  deriving
    ( -- | @since 1.0
      Bifunctor
    )
    via Either
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
instance (Semigroup a, Semigroup b) => Semigroup (Righty a b) where
  {-# INLINEABLE (<>) #-}
  Righty x <> Righty y = Righty $ case (x, y) of
    (Left x', Left y') -> Left $ x' <> y'
    (Left _, Right _) -> y
    (Right _, Left _) -> x
    (Right x', Right y') -> Right $ x' <> y'

-- | @since 1.0
instance (Monoid a, Semigroup b) => Monoid (Righty a b) where
  {-# INLINEABLE mempty #-}
  mempty = Righty . Left $ mempty

-- | @since 1.0
instance (Group a, Group b) => Group (Righty a b) where
  {-# INLINEABLE invert #-}
  invert = bimap invert invert

-- | @since 1.0
instance (Action a, Action b) => Action (Righty a b) where
  type TargetOf (Righty a b) = Either (TargetOf a) (TargetOf b)
  {-# INLINEABLE act #-}
  act (Righty x) = Endo $ case bimap act act x of
    Left (Endo f) -> first f
    Right (Endo g) -> second g
