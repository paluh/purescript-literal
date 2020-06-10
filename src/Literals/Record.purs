module Literals.Record where

import Prelude

import Literals.Reflect (class Reflect, reflect)
import Prim.Row (class Cons, class Lacks) as R
import Prim.RowList (Cons, Nil) as RL
import Prim.RowList (class RowToList, kind RowList)
import Record (insert) as Record
import Type.Prelude (class IsSymbol, Proxy(..), RLProxy(..), SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data RecordLit ∷ # Type → Type

instance reflectRecordLit ∷ (RowToList r rl, ReflectRL rl r') ⇒ Reflect (RecordLit r) (Record r') where
  reflect _ = reflectRL (RLProxy ∷ RLProxy rl)

class ReflectRL (rl ∷ RowList) (r ∷ #Type) | rl → r where
  reflectRL ∷ (RLProxy rl) → { | r }

instance reflectRLNil ∷ ReflectRL RL.Nil () where
  reflectRL _ = { }

instance reflectRLCons
  ∷ ( Reflect rep typ'
    , ReflectRL tail r'
    , R.Cons n typ' r' r''
    , IsSymbol n
    , R.Lacks n r'
    )
  ⇒ ReflectRL (RL.Cons n rep tail) r'' where
  reflectRL _ = Record.insert
    (SProxy ∷ SProxy n)
    (reflect (Proxy ∷ Proxy rep))
    (reflectRL (RLProxy ∷ RLProxy tail))

recordLit :: ∀ r row. Reflect (RecordLit row) r => RecordLit row
recordLit = unsafeCoerce $ reflect (Proxy ∷ Proxy (RecordLit row))

