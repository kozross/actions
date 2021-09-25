{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semigroup.Action
  ( Action (..),
    SelfActing (..),
  )
where

import Data.Bifunctor (bimap)
import Data.Group (Group)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup
  ( Endo (Endo),
    appEndo,
  )
import Data.These (These, these)
import Data.These.Combinators (bimapThese, mapHere, mapThere)

-- | A (minimally) semigroup action.
--
-- = Laws
--
-- Intuitively, @'act'@ must transform @w@ into changes of @TargetOf w@, while
-- preserving @w@'s \'structure\'. Specifically, the following must hold:
--
-- * @'act' w1 '<>' 'act' w2 = 'act' (w1 '<>' w2)
--
-- If @w@ is also a 'Monoid', then the following must hold too:
--
-- * @'act' 'mempty' = 'mempty'@
--
-- If @w@ is also a 'Group', then the following must hold too:
--
-- * @'act' w1 '<>' 'act' ('invert' w1) = 'act' ('invert' w1) '<>' 'act' w1 =
--   'mempty'
--
-- @since 1.0
class (Semigroup w) => Action (w :: Type) where
  type TargetOf w :: Type

  -- | @since 1.0
  act :: w -> Endo (TargetOf w)

-- | @since 1.0
newtype SelfActing (w :: Type) = SelfActing w
  deriving
    ( -- | @since 1.0
      Semigroup,
      -- | @since 1.0
      Monoid,
      -- | @since 1.0
      Group,
      -- | @since 1.0
      Eq
    )
    via w
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | If we want, we can have a 'Semigroup' (and indeed, a 'Monoid' or 'Group')
-- act on itself.
--
-- @since 1.0
instance (Semigroup w) => Action (SelfActing w) where
  type TargetOf (SelfActing w) = w
  {-# INLINEABLE act #-}
  act (SelfActing x) = Endo $ \y -> y <> x

-- | Since 'Proxy' is phantom in its last type parameter, it cannot describe any
-- changes.
--
-- @since 1.0
instance Action (Proxy a) where
  type TargetOf (Proxy a) = a
  {-# INLINEABLE act #-}
  act Proxy = mempty

-- | 'Endo' is self-describing.
--
-- @since 1.0
instance Action (Endo s) where
  type TargetOf (Endo s) = s
  {-# INLINEABLE act #-}
  act = id

-- | The product of (any number of) 'Action's can act on a product of their
-- targets.
--
-- @since 1.0
instance (Action w1, Action w2) => Action (w1, w2) where
  type TargetOf (w1, w2) = (TargetOf w1, TargetOf w2)
  {-# INLINEABLE act #-}
  act (x, y) =
    let Endo f = act x
        Endo g = act y
     in Endo $ bimap f g

-- | @since 1.0
instance (Action w1, Action w2) => Action (These w1 w2) where
  type TargetOf (These w1 w2) = These (TargetOf w1) (TargetOf w2)
  {-# INLINEABLE act #-}
  act =
    Endo
      . these
        (mapHere . go)
        (mapThere . go)
        (\x y -> bimapThese (go x) (go y))
    where
      go :: (Action w) => w -> TargetOf w -> TargetOf w
      go = appEndo . act
