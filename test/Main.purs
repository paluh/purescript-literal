module Test.Main where

import Prelude

import Data.Lens (set, view)
import Data.Lens.Record (prop) as Lens
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Foreign (Foreign, isUndefined)
import Literals (NumberLit, StringLit, IntLit, intLit, numberLit, stringLit)
import Literals.Literal (Literal)
import Literals.Record (RecordLit)
import Literals.Reflect (class Reflect, reflect)
import Literals.Undefined (undefined)
import Prim.Row (class Cons) as Row
import Prim.RowList (Cons, Nil) as RL
import Prim.RowList (class RowToList, kind RowList)
import Test.Assert (assertEqual, assertTrue)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (MapWithIndex)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (Proxy(..), RProxy(..), SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | An example implementation for default values on the type level
foreign import data Def ∷ Type → Symbol → Type

fromDef ∷ ∀ s t t'. Reflect (Literal t s) t' ⇒ Def t s → t'
fromDef d = if (isUndefined (unsafeCoerce d ∷ Foreign))
  then reflect (Proxy ∷ Proxy (Literal t s))
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


-- | An __really dirty__ example for generation record with lenses for a given record

foreign import data TypeExprProxy ∷ TypeExpr → Type

foreign import data Prop ∷ (Type → Type → Type) → Type → Type → TypeExpr

instance evalPropExpr ∷ Eval (Prop prof l x) (TypeExprProxy (Prop prof l x))



-- | I have problem with generation of rank-2 lens function in `Reflect`
-- | directly so I'm wrapping it here.

instance reflectProp
  ∷ (IsSymbol l, Row.Cons l a s_ s, Row.Cons l b s_ t, Strong prof)
  ⇒ Reflect (TypeExprProxy (Prop prof (SProxy l) x)) (prof a b → prof { | s } { | t }) where
  reflect _ = Lens.prop (SProxy ∷ SProxy l)

type ToProps prof = ToRow <<< MapWithIndex (Prop prof) <<< FromRow ∷ Type → TypeExpr

lenses ∷ ∀ lenses prof props profunctor s. Strong prof ⇒ Eval (ToProps prof (RProxy s)) (RProxy props) ⇒ Reflect (RecordLit props) lenses ⇒ RProxy s → lenses
lenses _ = reflect (Proxy ∷ Proxy (RecordLit props))

type ExampleRow =
  ( x ∷ String
  , y ∷ Int
  )

-- -- | Type signatures here are optional. They are here only for informative reasons ;-)

rLenses = lenses (RProxy ∷ RProxy ExampleRow)


getX ∷ String
getX =
  let
    r = { x: "test", y: 2 }
  in
    view rLenses.x r <> show (view rLenses.y (set rLenses.y 8.0 r))

getX' ∷ { x ∷ String , y ∷ Int } → String
getX' = view rLenses.x


reflectedRecord' ∷ Number
reflectedRecord' =
  let
    r = reflect (Proxy ∷ Proxy (RecordLit (x ∷ NumberLit "8.0")))
  in
    r.x

coerce ∷ ∀ g e. CoerceDef g e ⇒ g → e
coerce = unsafeCoerce

main ∷ Effect Unit
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
    { actual: unsafeCoerce (stringLit ∷ StringLit "foo")
    , expected: "foo"
    }
  assertEqual
    { actual: unsafeCoerce (numberLit ∷ NumberLit "8.0")
    , expected: 8.0
    }
  assertEqual
    { actual: unsafeCoerce (numberLit ∷ NumberLit "-8.0")
    , expected: -8.0
    }
  assertEqual
    { actual: unsafeCoerce (intLit ∷ IntLit "8")
    , expected: 8
    }
  assertEqual
    { actual: unsafeCoerce (intLit ∷ IntLit "-8")
    , expected: -8
    }

  -- Eq instance
  assertTrue $ undefined == undefined
  assertTrue $ (stringLit ∷ StringLit "foo") == (stringLit ∷ StringLit "foo")

  -- Show instance
  assertEqual
    { actual: show (stringLit ∷ StringLit "foo")
    , expected: "(Literal \"foo\")"
    }
  assertEqual
    { actual: show (numberLit ∷ NumberLit "8.0")
    , expected: "(Literal 8.0)"
    }
  assertEqual
    { actual: show (intLit ∷ IntLit "8")
    , expected: "(Literal 8)"
    }
  assertEqual
    { actual: show undefined
    , expected: "Undefined"
    }
