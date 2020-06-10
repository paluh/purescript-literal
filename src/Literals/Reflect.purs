module Literals.Reflect where

import Data.Symbol (class IsSymbol)
import Literals.Int (class Int, intLit)
import Literals.Literal (Literal, toValue)
import Literals.Null (Null, null)
import Literals.Number (class Number, numberLit)
import Literals.String (stringLit)
import Literals.Undefined (Undefined, undefined)
import Type.Prelude (Proxy)

class Reflect rep typ | rep → typ where
  reflect ∷ Proxy rep → typ

instance reflectNumber ∷ (IsSymbol s, Number s) ⇒ Reflect (Literal Number s) Number where
  reflect _ = toValue (numberLit ∷ Literal Number s)

instance reflectString ∷ (IsSymbol s) ⇒ Reflect (Literal String s) String where
  reflect _ = toValue (stringLit ∷ Literal String s)

instance reflectInt ∷ (IsSymbol s, Int s) ⇒ Reflect (Literal Int s) Int where
  reflect _ = toValue (intLit ∷ Literal Int s)

instance reflectBooleanTrue ∷ (IsSymbol s, Int s) ⇒ Reflect (Literal Boolean "true") Boolean where
  reflect _ = true

instance reflectBooleanFalse ∷ (IsSymbol s, Int s) ⇒ Reflect (Literal Boolean "false") Boolean where
  reflect _ = false

instance reflectNull ∷ Reflect Null Null where
  reflect _ = null

instance reflectUndefined ∷ Reflect Undefined Undefined where
  reflect _ = undefined

