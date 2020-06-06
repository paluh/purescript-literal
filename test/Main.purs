module Test.Main where

import Prelude

import Effect (Effect)
import Foreign (Foreign, isUndefined)
import Literals (NumberLit, StringLit, IntLit, intLit, numberLit, stringLit)
import Literals.Literal (Literal)
import Literals.Record (RecordLit)
import Literals.Reflect (class Reflect, reflect)
import Literals.Undefined (undefined)
import Prim.RowList (Cons, Nil) as RL
import Prim.RowList (class RowToList, kind RowList)
import Record (get) as Record
import Test.Assert (assertEqual, assertTrue)
import Type.Prelude (Proxy(..), SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | I gave Literal different name for clarity
foreign import data Def ∷ Type → Symbol → Type

fromDef ∷ ∀ s t t'. Reflect t (SProxy s) t' ⇒ Def t s → t'
fromDef d = if (isUndefined (unsafeCoerce d ∷ Foreign))
  then reflect (Proxy ∷ Proxy (Literal t (SProxy s)))
  else unsafeCoerce d

class CoerceDef given expected

instance coerceDef ∷ (CoerceDef a b) ⇒ CoerceDef a (Def b s)
else instance coerceRercords ∷ (RowToList g gl, RowToList e el, CoerceDefRL gl el) ⇒ CoerceDef { | g } { | e }
else instance coerceValue ∷ CoerceDef a a

class CoerceDefRL (given ∷ RowList) (expected ∷ RowList)

instance coerceDefRLNil ∷ CoerceDefRL RL.Nil RL.Nil
else instance coerceDefRLCons ∷ (CoerceDef a b, CoerceDefRL t t') ⇒ CoerceDefRL (RL.Cons n a t) (RL.Cons n b t')
else instance coerceDefRLMissing ∷ (CoerceDefRL t t') ⇒ CoerceDefRL t (RL.Cons n (Def a s) t')

type R =
  { x ∷ Def Number "8.0"
  , y ∷ Def String "default"
  }

reflectedRecord' :: Number
reflectedRecord' =
  let
    r = reflect (Proxy ∷ Proxy (RecordLit (x ∷ NumberLit "8.0")))
  in
    r.x

coerce ∷ ∀ g e. CoerceDef g e ⇒ g → e
coerce = unsafeCoerce

main :: Effect Unit
main = do
  let
    r = coerce { y: "non default" } ∷ R
  assertEqual
    { actual: fromDef r.x
    , expected: 8.0
    }
  assertEqual
    { actual: fromDef r.y
    , expected: "non default"
    }
  assertEqual
    { actual: unsafeCoerce (stringLit :: StringLit "foo")
    , expected: "foo"
    }
  assertEqual
    { actual: unsafeCoerce (numberLit :: NumberLit "8.0")
    , expected: 8.0
    }
  assertEqual
    { actual: unsafeCoerce (numberLit :: NumberLit "-8.0")
    , expected: -8.0
    }
  assertEqual
    { actual: unsafeCoerce (intLit :: IntLit "8")
    , expected: 8
    }
  assertEqual
    { actual: unsafeCoerce (intLit :: IntLit "-8")
    , expected: -8
    }

  -- Eq instance
  assertTrue $ undefined == undefined
  assertTrue $ (stringLit :: StringLit "foo") == (stringLit :: StringLit "foo")

  -- Show instance
  assertEqual
    { actual: show (stringLit :: StringLit "foo")
    , expected: "(Literal \"foo\")"
    }
  assertEqual
    { actual: show (numberLit :: NumberLit "8.0")
    , expected: "(Literal 8.0)"
    }
  assertEqual
    { actual: show (intLit :: IntLit "8")
    , expected: "(Literal 8)"
    }
  assertEqual
    { actual: show undefined
    , expected: "Undefined"
    }
