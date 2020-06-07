module Test.Main where

import Prelude

import Data.Lens (Lens, Lens', Optic, view)
import Data.Lens.Record (prop) as Lens
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Foreign (Foreign, isUndefined)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
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
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (Proxy(..), RLProxy(..), RProxy(..), SProxy(..))
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


-- | An __really dirty__ example for generation record with lenses for a given record

foreign import data TypeExprProxy ∷ TypeExpr → Type

foreign import data Prop ∷ Type → Type → Type → Type → TypeExpr

instance evalPropExpr ∷ Eval (Prop s t_ l a) (TypeExprProxy (Prop s t_ l a))


foreign import data MapWithIndex' ∷ Type → (Type → Type → Type → TypeExpr) → Type → TypeExpr

instance mapWithIndex_RowList_Cons' ::
  ( Eval (fn (RProxy orig_) (SProxy sym) a) b
  , Row.Cons sym a orig_ orig
  , Eval (MapWithIndex' (RProxy orig) fn (RLProxy rl)) (RLProxy rl')
  ) =>
  Eval (MapWithIndex' (RProxy orig) fn (RLProxy (RL.Cons sym a rl))) (RLProxy (RL.Cons sym b rl'))

instance mapWithIndex_RowList_Nil' ::
  Eval (MapWithIndex' orig fn (RLProxy RL.Nil)) (RLProxy RL.Nil)


-- | I have problem with generation of rank-2 lens function in `Reflect`
-- | directly so I'm wrapping it here.
newtype LensWrapper a b s t = LensWrapper (∀ p. Strong p ⇒ p a b → p s t)

instance reflectProp
  ∷ (IsSymbol l, Row.Cons l b t_ t, Row.Cons l a t_ s)
  ⇒ Reflect (TypeExprProxy (Prop (RProxy s) (RProxy t_) (SProxy l) a)) (LensWrapper a b { | s } { | t }) where
  reflect _ = LensWrapper (Lens.prop (SProxy ∷ SProxy l))

type X s = MapWithIndex' s (Prop s) ∷ Type → TypeExpr

type ToProps s = ToRow <<< (X s) <<< FromRow ∷ Type → TypeExpr

lenses ∷ ∀ lenses props s. Eval (ToProps (RProxy s) (RProxy s)) (RProxy props) ⇒ Reflect (RecordLit props) lenses ⇒ RProxy s → lenses
lenses _ = reflect (Proxy ∷ Proxy (RecordLit props))

unWrapLens (LensWrapper l) = l

type ExampleRow =
  ( x ∷ String
  , y ∷ Int
  )

-- -- | Type signatures here are optional. They are here only for informative reasons ;-)

rLenses ∷ ∀ t49 t54.
   { x ∷ LensWrapper
      String
      t49
      { x ∷ String
      , y ∷ Int
      }
      { x ∷ t49
      , y ∷ Int
      }
   , y ∷ LensWrapper
      Int
      t54
      { x ∷ String
      , y ∷ Int
      }
      { x ∷ String
      , y ∷ t54
      }
   }
rLenses = lenses (RProxy ∷ RProxy ExampleRow)


-- | Usage - unfortunatelly I'm not able to drop LensWrapper

getX ∷ String
getX = view (unWrapLens rLenses.x) { x: "test", y: 2 }

getX' ∷ { x ∷ String , y ∷ Int } → String
getX' = view (unWrapLens rLenses.x)


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
