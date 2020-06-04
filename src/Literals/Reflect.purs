module Literals.Reflect where

import Data.Symbol (class IsSymbol)
import Literals.Int (class Int, intLit)
import Literals.Literal (Literal, toValue)
import Literals.Null (Null, null)
import Literals.Number (class Number, numberLit)
import Literals.String (stringLit)
import Literals.Undefined (Undefined, undefined)
import Type.Prelude (Proxy)

class Reflect t (s ∷ Symbol) where
  reflect ∷ Proxy (Literal t s) → t

instance reflectNumber ∷ (IsSymbol s, Number s) ⇒ Reflect Number s where
  reflect _ = toValue (numberLit ∷ Literal Number s)

instance reflectString ∷ (IsSymbol s) ⇒ Reflect String s where
  reflect _ = toValue (stringLit ∷ Literal String s)

instance reflectInt ∷ (IsSymbol s, Int s) ⇒ Reflect Int s where
  reflect _ = toValue (intLit ∷ Literal Int s)

instance reflectBooleanTrue ∷ (IsSymbol s, Int s) ⇒ Reflect Boolean "true" where
  reflect _ = true

instance reflectBooleanFalse ∷ (IsSymbol s, Int s) ⇒ Reflect Boolean "false" where
  reflect _ = false

instance reflectNull ∷ (IsSymbol s) ⇒ Reflect Null s where
  reflect _ = null

instance reflectUndefined ∷ (IsSymbol s) ⇒ Reflect Undefined s where
  reflect _ = undefined
