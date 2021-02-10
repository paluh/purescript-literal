module Literals.Generic where

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), to)
import Literals.Reflect (class Reflect, reflect)
import Type.Prelude (Proxy(..), SProxy(..))

foreign import data Inl ∷ Type → Type
foreign import data Inr ∷ Type → Type

-- | output type → constructor name → arguments → literal
foreign import data GenericLit ∷ Type → Symbol → Type → Type

instance reflectGenericLit ∷ (Generic t rep, ReflectGeneric rep n args) ⇒ Reflect (GenericLit t n args) t where
  reflect _ = to (reflectGeneric (Proxy ∷ Proxy rep) (SProxy ∷ SProxy n) (Proxy ∷ Proxy args))

class ReflectGeneric rep (name ∷ Symbol) args where
  reflectGeneric ∷ (Proxy rep) → (SProxy name) → (Proxy args) → rep

instance reflectGenericConstructor ∷ (Reflect args args') ⇒ ReflectGeneric (Constructor n args') n args where
  reflectGeneric _ _ _ = Constructor (reflect (Proxy ∷ Proxy args)) ∷ Constructor n args'

instance reflectGenericSumInl ∷ (Reflect args args') ⇒ ReflectGeneric (Sum (Constructor n args') t) n args where
  reflectGeneric _ _ _ = Inl (Constructor (reflect (Proxy ∷ Proxy args)) ∷ Constructor n args')

else instance reflectGenericSumInr ∷ (ReflectGeneric t n args) ⇒ ReflectGeneric (Sum c t) n args where
  reflectGeneric _ _ _ = Inr (reflectGeneric (Proxy ∷ Proxy t) (SProxy ∷ SProxy n) (Proxy ∷ Proxy args))

foreign import data ProductLit ∷ Type → Type → Type
foreign import data ArgumentLit ∷ Type → Type
foreign import data NoArgumentsLit ∷ Type

instance reflectProduct ∷ (Reflect a a', Reflect t t') ⇒ Reflect (ProductLit a t) (Product a' t') where
  reflect _ = Product (reflect (Proxy ∷ Proxy a)) (reflect (Proxy ∷ Proxy t))

instance reflectArgument ∷ (Reflect a a') ⇒ Reflect (ArgumentLit a) (Argument a') where
  reflect _ = Argument (reflect (Proxy ∷ Proxy a))

instance reflectNoArgumentsLit ∷ Reflect NoArgumentsLit NoArguments where
  reflect _ = NoArguments



