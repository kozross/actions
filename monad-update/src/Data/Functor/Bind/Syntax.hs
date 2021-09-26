{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Functor.Bind.Syntax
  ( (>>=),
  )
where

import Data.Functor.Bind (Bind ((>>-)))
import Data.Kind (Type)
import Prelude hiding ((>>=))

(>>=) ::
  forall (m :: Type -> Type) (a :: Type) (b :: Type).
  (Bind m) =>
  m a ->
  (a -> m b) ->
  m b
(>>=) = (>>-)
