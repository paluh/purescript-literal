module Literals.Record where

import Literals (Literal)
import Literals.Reflect (class Reflect)
import Prim.RowList (class RowToList, kind RowList)
import Type.Prelude (RProxy)
import Type.RowList (RLProxy(..))

type RecordLit r = Literal (RProxy r) (RProxy r)

