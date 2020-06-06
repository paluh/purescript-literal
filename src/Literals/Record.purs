module Literals.Record where

import Literals (Literal)
import Literals.Reflect (class Reflect)
import Prim.RowList (class RowToList, kind RowList)
import Type.Prelude (RProxy)
import Type.RowList (RLProxy(..))

type RecordLit r = Literal (RProxy r) (RProxy r)

-- class Record (head :: Symbol) (tail :: Symbol) (part :: NumberPart)
-- 
-- instance signPart :: (Cons h' t' t, Number' h' t' IntegerPart) => Number' "-" t Sign
-- else instance signPartEmpty :: (Number' h t IntegerPart) => Number' h t Sign
-- else instance pointIntegerPart :: (Cons h' t' t, Number' h' t' FractionalPart) => Number' "." t IntegerPart
-- else instance lastFractionalPart :: (Digit h) => Number' h "" FractionalPart
-- else instance digitIntegerPart :: (Digit h, Cons h' t' t, Number' h' t' IntegerPart) => Number' h t IntegerPart
-- else instance digitFractionalPart :: (Digit h, Cons h' t' t, Number' h' t' FractionalPart) => Number' h t FractionalPart

-- r = prop (SProxy ∷ SProxy "test")

-- recordLit :: forall sym. IsSymbol sym => Number sym => RecordLit r
-- recordLit = reflect r
