{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semigroup.Action
  ( -- * Type class
    Action (..),

    -- * Helper functions
    action,
    mu,
    react,
  )
where

import Data.Bifunctor (bimap, first, second)
import Data.Can (Can, can)
import Data.Functor.Identity (Identity (Identity))
import Data.Group (Group (invert))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup
  ( Endo (Endo),
    appEndo,
  )
import Data.Smash (Smash, smash)
import Data.These (These, these)
import Data.Wedge (Wedge, wedge)

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

-- | If we want, we can have a 'Semigroup' (and indeed, a 'Monoid' or
-- 'Group') act on itself.
--
-- @since 1.0
instance (Semigroup w) => Action (Identity w) where
  type TargetOf (Identity w) = w
  {-# INLINEABLE act #-}
  act (Identity x) = Endo (<> x)

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

-- | A 'These' of 'Action's can act on a 'These' of their targets.
--
-- @since 1.0
instance (Action w1, Action w2) => Action (These w1 w2) where
  type TargetOf (These w1 w2) = These (TargetOf w1) (TargetOf w2)
  {-# INLINEABLE act #-}
  act =
    Endo
      . these
        (first . action)
        (second . action)
        (\x y -> bimap (action x) (action y))

-- | A 'Can' of 'Action's can act on a 'Can' of their targets.
--
-- @since 1.0
instance (Action w1, Action w2) => Action (Can w1 w2) where
  type TargetOf (Can w1 w2) = Can (TargetOf w1) (TargetOf w2)
  {-# INLINEABLE act #-}
  act =
    Endo
      . can
        id
        (first . action)
        (second . action)
        (\x y -> bimap (action x) (action y))

-- | A 'Smash' of 'Action's can act on a 'Smash' of their targets.
--
-- @since 1.0
instance (Action w1, Action w2) => Action (Smash w1 w2) where
  type TargetOf (Smash w1 w2) = Smash (TargetOf w1) (TargetOf w2)
  {-# INLINEABLE act #-}
  act = Endo . smash id (\x y -> bimap (action x) (action y))

-- | A 'Wedge' of 'Action's can act on a 'Wedge' of their targets.
--
-- @since 1.0
instance (Action w1, Action w2) => Action (Wedge w1 w2) where
  type TargetOf (Wedge w1 w2) = Wedge (TargetOf w1) (TargetOf w2)
  {-# INLINEABLE act #-}
  act = Endo . wedge id (first . action) (second . action)

-- | Get the action as a function, rather than an 'Endo'.
--
-- @since 1.0
action ::
  forall (w :: Type).
  (Action w) =>
  w ->
  TargetOf w ->
  TargetOf w
action = appEndo . act

-- | Short for @'act' 'mempty'@.
--
-- @since 1.0
mu ::
  forall (w :: Type).
  (Action w, Monoid w) =>
  Endo (TargetOf w)
mu = act @w mempty

-- | Short for @'act' '.' invert'@.
--
-- @since 1.0
react ::
  forall (w :: Type).
  (Group w, Action w) =>
  w ->
  Endo (TargetOf w)
react = act . invert
