module Literals.Record where

import Literals (Literal)
import Prim.RowList (class RowToList, kind RowList)
import Type.Prelude (RProxy)
import Type.RowList (RLProxy(..))

foreign import data RecordLit ∷ (# Type) → Type


