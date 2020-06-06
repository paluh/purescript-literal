module Test.Main where

import Prelude

import Data.Lens (Lens, Lens', Optic, view)
import Data.Lens.Record (prop) as Lens
import Data.Newtype (class Newtype, unwrap)
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
import Record (get) as Record
import Test.Assert (assertEqual, assertTrue)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (MapWithIndex)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (Proxy(..), RProxy(..), SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | An example implementation for default values on the type level
foreign import data Def ∷ Type → Symbol → Type

fromDef ∷ ∀ s t t'. Reflect (Literal t (SProxy s)) t' ⇒ Def t s → t'
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


-- | An dirty example for generation record with lenses for a given record

foreign import data TypeExprProxy ∷ TypeExpr → Type

foreign import data Prop ∷ (# Type) → Type → Type → TypeExpr

instance evalPropExpr ∷ Eval (Prop r s a) (TypeExprProxy (Prop r s a))

-- | I have problem with generation of rank-2 lens function in `Reflect`
-- | directly so I'm wrapping it here.
newtype LensWrapper a r = LensWrapper (∀ p. Strong p ⇒ p a a → p r r)

-- | I'm not able to use Lens alias here - I have to use its
-- | function signature and constraint directly
instance reflectProp
  ∷ (IsSymbol s, Row.Cons s a r' r)
  ⇒ Reflect (TypeExprProxy (Prop r (SProxy s) a)) (LensWrapper a { | r }) where
  reflect _ = LensWrapper (Lens.prop (SProxy ∷ SProxy s))

type ToProps r = ToRow <<< MapWithIndex (Prop r) <<< FromRow

lenses ∷ ∀ lenses props r. Eval (ToProps r (RProxy r)) (RProxy props) ⇒ Reflect (RecordLit props) lenses ⇒ RProxy r → lenses
lenses _ = reflect (Proxy ∷ Proxy (RecordLit props))

unWrap (LensWrapper l) = l

type ExampleRow =
  ( x ∷ String
  , y ∷ Int
  )

type ExampeRecord = { | ExampleRow }

rLenses = lenses (RProxy ∷ RProxy ExampleRow)

-- | Type signatures here are optional. Only informative reasons ;-)

getX :: String
getX = view (unWrap rLenses.x) { x: "test", y: 2 }

getX' :: { x :: String , y :: Int } -> String
getX' = view (unWrap rLenses.x)


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
