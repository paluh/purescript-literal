module Test.RunCodegen where

import Prelude
import Control.Monad.Free (Free, liftF)
import Data.Functor.Variant (FProxy(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Product(..), Sum(..))
import Data.Generic.Rep (to) as Generics.Rep
import Data.Symbol (class IsSymbol)
import Prelude (Unit, identity, unit, ($))
import Prim.Row (class Cons, class Lacks) as Row
import Prim.Symbol (class Append, class Cons) as Symbol
import Record (insert) as Record
import Type.Prelude (SProxy(..))
import Type.Proxy (Proxy(..))

class LowerFirst (i ∷ Symbol) (o ∷ Symbol) | i → o

instance lowerFirst ∷
  ( Symbol.Cons l s i
  , LowerCase l l'
  , Symbol.Cons l' s o
  ) ⇒
  LowerFirst i o

-- | TODO: Handles only symbols required by the example below ;-)
class LowerCase (i ∷ Symbol) (o ∷ Symbol) | i → o

instance lowerCaseD ∷ LowerCase "D" "d"

instance lowerCaseG ∷ LowerCase "G" "g"

instance lowerCaseR ∷ LowerCase "R" "r"

instance lowerCaseU ∷ LowerCase "U" "u"

foreign import kind ConstructorPath

foreign import data Top ∷ ConstructorPath

foreign import data Inl ∷ ConstructorPath → ConstructorPath

foreign import data Inr ∷ ConstructorPath → ConstructorPath

data PProxy (path ∷ ConstructorPath)
  = PProxy

class ReconstructGeneric (path ∷ ConstructorPath) a g | path a → g where
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
  genericFreeConstructor ∷ FProxy t → Proxy g → PProxy p → { | rin } → { | rout }

instance genericFreeConstructorSum ::
  ( GenericFreeConstructor t l (Inl p) rin lout
  , GenericFreeConstructor t r (Inr p) lout rout
  ) =>
  GenericFreeConstructor t (Sum l r) p rin rout where
  genericFreeConstructor fp _ _ rin = rout
    where
    lout = genericFreeConstructor fp (Proxy ∷ Proxy l) (PProxy ∷ PProxy (Inl p)) rin

    rout = genericFreeConstructor fp (Proxy ∷ Proxy r) (PProxy ∷ PProxy (Inr p)) lout
else instance genericFreeConstructorSingleParamEff ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (a → Free t args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument a) (Argument (args → args)))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor t (Constructor name (Product (Argument a) (Argument (args' → Unit)))) p rin rout where
  genericFreeConstructor _ _ p rin = Record.insert (SProxy ∷ SProxy name') f rin
    where
    f a = liftF $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument a) (Argument identity))) ∷ Constructor name (Product (Argument a) (Argument (args → args))))) ∷ t args)
else instance genericFreeConstructorNoParamEff ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (Free t args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Argument (args → args))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor t (Constructor name (Argument (args' → Unit))) p rin rout where
  genericFreeConstructor _ _ p rin = Record.insert (SProxy ∷ SProxy name') f rin
    where
    f = liftF $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Argument identity)) ∷ Constructor name (Argument (args → args)))) ∷ t args)
else instance genericFreeConstructorThreeParamUnitEff ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (a → b → Free t args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument a) (Product (Argument b) (Argument Unit)))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor t (Constructor name (Product (Argument a) (Product (Argument b) (Argument Unit)))) p rin rout where
  genericFreeConstructor _ _ p rin = Record.insert (SProxy ∷ SProxy name') f rin
    where
    f a b = liftF $ (Generics.Rep.to (reconstructGeneric p ((Constructor (Product (Argument a) (Product (Argument b) (Argument unit)))) ∷ Constructor name (Product (Argument a) (Product (Argument b) (Argument Unit))))) ∷ t args)
else instance genericFreeConstructorFourParamsUnitEff ::
  ( LowerFirst name name'
  , IsSymbol name'
  , Row.Cons name' (a → b → c → Free t args) rin rout
  , Row.Lacks name' rin
  , IsSymbol name
  , ReconstructGeneric p (Constructor name (Product (Argument a) (Product (Argument b) (Product (Argument c) (Argument Unit))))) g'
  , Generic (t args) g'
  ) =>
  GenericFreeConstructor t (Constructor name (Product (Argument a) (Product (Argument b) (Product (Argument c) (Argument Unit))))) p rin rout where
  genericFreeConstructor _ _ p rin = Record.insert (SProxy ∷ SProxy name') f rin
    where
    f a b c =
      liftF
        $ Generics.Rep.to
        $ reconstructGeneric p
        $ build a b c

    build ∷ a → b → c → Constructor name (Product (Argument a) (Product (Argument b) (Product (Argument c) (Argument Unit))))
    build a b c = Constructor $ Product (Argument a) $ Product (Argument b) $ Product (Argument c) (Argument unit)

constructors ∷ ∀ g rout t. Generic (t Unit) g ⇒ GenericFreeConstructor t g Top () rout ⇒ FProxy t → { | rout }
constructors fp = genericFreeConstructor fp (Proxy ∷ Proxy g) (PProxy ∷ PProxy Top) {}

-- | Functor definition
data S3SquirrelProgramF a
  = GetETagHeaderForResource String (String → a)
  | DownloadResourceToFile String String a
  | ReadFileToBuffer String (Int → a)
  | UploadObjectToS3 String String Int a
  | GenerateUUID (String → a)

-- | `Generic` instance is required
derive instance genericS3SquirrelProgramF ∷ Generic (S3SquirrelProgramF a) _

-- | This signature is optional
c ::
  { downloadResourceToFile :: String -> String -> Free S3SquirrelProgramF Unit
  , generateUUID :: Free S3SquirrelProgramF String
  , getETagHeaderForResource :: String -> Free S3SquirrelProgramF String
  , readFileToBuffer :: String -> Free S3SquirrelProgramF Int
  , uploadObjectToS3 :: String -> String -> Int -> Free S3SquirrelProgramF Unit
  }
c = constructors (FProxy ∷ FProxy S3SquirrelProgramF)

program = do
  c.downloadResourceToFile "https://example.com" "test"
  uuid ← c.generateUUID
  c.uploadObjectToS3 "foo" "bar" 8
