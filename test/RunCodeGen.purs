module Test.RunCodegen where

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..))
import Prelude (Unit, identity, ($))
import Control.Monad.Free (Free, liftF)
import Data.Functor.Variant (FProxy(..))
import Data.Generic.Rep (to) as Generics.Rep
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks) as Row
import Record (get, insert) as Record
import Type.Prelude (SProxy(..))
import Type.Proxy (Proxy(..))

foreign import kind ConstructorPath

foreign import data Top ∷ ConstructorPath

foreign import data Inl ∷ ConstructorPath → ConstructorPath

foreign import data Inr ∷ ConstructorPath → ConstructorPath

data PProxy (path ∷ ConstructorPath)
  = PProxy

class ReconstructGeneric (path ∷ ConstructorPath) a g | path a -> g where
  reconstructGeneric ∷ PProxy path → a → g

instance reconstructGenericTop ∷ ReconstructGeneric Top a a where
  reconstructGeneric _ a = a
else instance reconstructGenericInl ∷
  (ReconstructGeneric p (Sum a t) b) ⇒
  ReconstructGeneric (Inl p) a b where
  reconstructGeneric _ a = reconstructGeneric (PProxy ∷ PProxy p) (Inl a ∷ Sum a t)
else instance reconstructGenericInr ∷
  (ReconstructGeneric p (Sum t a) b) ⇒
  ReconstructGeneric (Inr p) a b where
  reconstructGeneric _ a = reconstructGeneric (PProxy ∷ PProxy p) (Inr a ∷ Sum t a)

class GenericFreeConstructor (t ∷ Type → Type) g (p ∷ ConstructorPath) rin rout | t g → rin rout p where
  genericFreeConstructor :: FProxy t → Proxy g → PProxy p → { | rin } → { | rout }

instance genericFreeConstructorSingleFunArg ::
  ( IsSymbol name
  , Row.Cons name (Free t args) rin rout
  , Row.Lacks name rin
  , ReconstructGeneric p (Constructor name (Argument (args → args))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor t (Constructor name (Argument (args' → Unit))) p rin rout where
  genericFreeConstructor _ _ p rin = Record.insert (SProxy ∷ SProxy name) f rin
    where
    f = liftF $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Argument identity)) ∷ Constructor name (Argument (args → args)))) ∷ t args)
else instance genericFreeConstructorSum ::
  ( GenericFreeConstructor t l (Inl p) rin lout
  , GenericFreeConstructor t r (Inr p) lout rout
  ) =>
  GenericFreeConstructor t (Sum l r) p rin rout where
  genericFreeConstructor fp _ _ rin = rout
    where
    lout = genericFreeConstructor fp (Proxy ∷ Proxy l) (PProxy ∷ PProxy (Inl p)) rin

    rout = genericFreeConstructor fp (Proxy ∷ Proxy r) (PProxy ∷ PProxy (Inr p)) lout

constructors ∷ ∀ g rout t. Generic (t Unit) g ⇒ GenericFreeConstructor t g Top () rout ⇒ FProxy t → { | rout }
constructors fp = genericFreeConstructor fp (Proxy ∷ Proxy g) (PProxy ∷ PProxy Top) {}

-- | Functor definition
data S3SquirrelProgramF a
  = GenerateUUID (String -> a)
  | GetRandomInt (Int -> a)

-- | `Generic` instance is required
derive instance genericS3SquirrelProgramF ∷ Generic (S3SquirrelProgramF a) _

-- | This signature is optional
y ::
  { "GenerateUUID" :: Free S3SquirrelProgramF String
  , "GetRandomInt" :: Free S3SquirrelProgramF Int
  }
y = (constructors (FProxy ∷ FProxy S3SquirrelProgramF))
