module Literals.Reflect where

import Data.Symbol (class IsSymbol)
import Literals.Int (class Int, intLit)
import Literals.Literal (Literal, toValue)
import Literals.Null (Null, null)
import Literals.Number (class Number, numberLit)
import Literals.String (stringLit)
import Literals.Undefined (Undefined, undefined)
import Prim.Row (class Cons, class Lacks) as R
import Prim.RowList (Cons, Nil) as RL
import Prim.RowList (class RowToList, kind RowList)
import Record (insert) as Record
import Type.Prelude (Proxy(..), RLProxy(..), SProxy(..))
import Type.Row (RProxy)

class Reflect typ rep typ' | typ rep → typ' where
  reflect ∷ Proxy (Literal typ rep) → typ'

instance reflectNumber ∷ (IsSymbol s, Number s) ⇒ Reflect Number (SProxy s) Number where
  reflect _ = toValue (numberLit ∷ Literal Number (SProxy s))

instance reflectString ∷ (IsSymbol s) ⇒ Reflect String (SProxy s) String where
  reflect _ = toValue (stringLit ∷ Literal String (SProxy s))

instance reflectInt ∷ (IsSymbol s, Int s) ⇒ Reflect Int (SProxy s) Int where
  reflect _ = toValue (intLit ∷ Literal Int (SProxy s))

instance reflectBooleanTrue ∷ (IsSymbol s, Int s) ⇒ Reflect Boolean (SProxy "true") Boolean where
  reflect _ = true

instance reflectBooleanFalse ∷ (IsSymbol s, Int s) ⇒ Reflect Boolean (SProxy "false") Boolean where
  reflect _ = false

instance reflectNull ∷ Reflect Null any Null where
  reflect _ = null

instance reflectUndefined ∷ Reflect Undefined any Undefined where
  reflect _ = undefined

instance reflectRecord ∷ (RowToList r rl, ReflectRL rl r') ⇒ Reflect (RProxy r) (RProxy r) (Record r') where
  reflect _ = reflectRL (RLProxy ∷ RLProxy rl)

class ReflectRL (rl ∷ RowList) (r ∷ #Type) | rl → r where
  reflectRL ∷ (RLProxy rl) → { | r }

instance reflectRLNil ∷ ReflectRL RL.Nil () where
  reflectRL _ = { }

instance reflectRLCons
  ∷ ( Reflect typ rep typ'
    , ReflectRL tail r'
    , R.Cons n typ' r' r''
    , IsSymbol n
    , R.Lacks n r'
    )
  ⇒ ReflectRL (RL.Cons n (Literal typ rep) tail) r'' where
  reflectRL _ = Record.insert
    (SProxy ∷ SProxy n)
    (reflect (Proxy ∷ Proxy (Literal typ rep)))
    (reflectRL (RLProxy ∷ RLProxy tail))

