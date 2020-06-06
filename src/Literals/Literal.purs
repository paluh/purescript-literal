module Literals.Literal where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data Literal :: Type -> Type -> Type

instance literalEq :: Eq (Literal typ rep) where
  eq _ _ = true
instance literalShow :: Show typ => Show (Literal typ rep) where
  show a = "(Literal " <> (show $ toValue a) <> ")"

toValue :: ∀ a r. Literal a r -> a
toValue = unsafeCoerce
