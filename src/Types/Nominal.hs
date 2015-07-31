{-# LANGUAGE DeriveDataTypeable, OverloadedLists, OverloadedStrings, ViewPatterns, MultiParamTypeClasses,  FunctionalDependencies, ScopedTypeVariables, FlexibleContexts, AllowAmbiguousTypes, DefaultSignatures, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances, TemplateHaskell, AutoDeriveTypeable #-}
module Types.Nominal where
  import Prelude 
  import Numeric.Natural
  import qualified Data.Text as T
  import Data.Sequence
  import Data.Map
  import Data.Foldable
  import Data.Traversable
  import Data.Monoid
--  import Generics.Deriving.Uniplate hiding (universe)
--  import GHC.Generics

  import Control.Lens hiding (universe)
  import Control.Monad.RWS


    -- A name has several properties. Some, like the actual identity and universe, are intrinsic. Others, like a human-friendly name and the intended purpose, reflect the metatheoretic properties.
  data Name = Name { _friendlyName :: T.Text, _ident :: Natural, _universe :: Maybe Name }

  instance Eq Name where
    (Name _ a ua ) == (Name _ b ub) =  a == b && ua == ub

  -- Intended for usage with containers for efficient storage. Do not use for actual equality testing.
  instance Ord Name where
    left <= right = case (_universe left, _universe right) of
                       (Just lu, Just ru) -> (lu <= ru) && (_ident left) <= (_ident right)
                       (Nothing, Just ru) -> False
                       (Just lu, Nothing) -> True
                       _ -> (_ident left) <= (_ident right)
  instance Show Name where
    show (Name n _ _) = show n

  makeClassy ''Name
  newtype Permutation = Permutation (Map Name Name) deriving (Eq, Show)
  
  instance Monoid Permutation where
    mempty = Permutation Data.Map.empty
    (Permutation second) `mappend` (Permutation first) = Permutation merged where
      (overwritten,normal) = Data.Map.partition (\x -> member x second) first
      withUpdated = Data.Map.map ((!) second) overwritten
      merged = unions [normal, withUpdated, second]
        
  class TermAlgebra a where
    toTerm :: a -> NominalTerm a
    fromTerm :: NominalTerm a -> a
  --    support :: a -> f Name
  
  newtype TermFormerID = TermFormerID Natural deriving (Eq, Show)
  -- Used for dynamic sequences of name usage. For example, memory locations in an imperitive program. Models binding arities.
  data BindingToken a = Create Name
                 | Destroy Name
                 | RawTerm a
                 | Constant a deriving (Eq, Show, Functor, Foldable, Traversable)


  data NominalTerm a = Atom Name
                   | Variable Permutation Name
                   | BindingSequence [BindingToken a]
                   | TermFormer TermFormerID [NominalTerm a] deriving (Eq, Show, Functor,  Foldable, Traversable)


  makeClassy ''BindingToken
  makeClassyPrisms ''BindingToken
  makeClassy ''NominalTerm
  makeClassyPrisms ''NominalTerm
  type NominalState = Natural
  type NominalMonad r w = RWS r w NominalState

  

  freshvar ::  Monoid w => NominalMonad r w Name
  freshvar = do n <- get
                put (n+1)
                return (Name "" n Nothing)



  
 









  data Lambda = Var Name
              | Lam Name Lambda
              | App Lambda Lambda deriving (Eq)

  instance Show Lambda where
    show (Var n) = show n
    show (Lam n l) = 'λ' : show n ++  '.' : (show l)
    show (App l r) = '(' : show l ++ ')' : show r

  instance TermAlgebra Lambda where
    toTerm (Var x) = Atom x
    toTerm (Lam x f) = BindingSequence [Create x, RawTerm f, Destroy x]
    toTerm (App x y) = TermFormer (TermFormerID 1) [toTerm x, toTerm y]

    fromTerm (Atom x) = Var x
    fromTerm (BindingSequence [Create x, RawTerm f, Destroy y]) | x == y = (Lam x f)
    fromTerm (TermFormer (TermFormerID 1) [x, y]) = App (fromTerm x) (fromTerm y)

  makeName n nu = Name n nu Nothing
  exLambda = App (Lam (makeName "x" 1) (Var(makeName "x" 1))) (Var (makeName "y" 2))


  makeClassy ''Lambda
  makeClassyPrisms ''Lambda
{-


 -- A name can be data - like a variable - or an identifier - like "NameType".
  data NameType = Identifier | Data

  {-# DEPRECATED binder "remove binder, or at least implement it in terms of bindSequences" #-}
  -- Nominal types.
  class (Generic a)  => Nominal f a | a -> f where
    -- The support of the type. The support can be thought of as the list of variables in an abstract syntax tree.
    support :: Traversable f => a -> f Name
    
    default support :: (Monoid (f Name), Traversable f) => a -> f Name
    support (binder -> Nothing) = mempty
    support (binder -> Just (names, children)) = names <> foldMap support children 
                                                   
        
    -- Swaps one name for another.
    swap :: a -> (Name, Name) -> a
    -- Is an element of a type a binder? For example, in the untyped lambda calculus, "Var x" would return Just (singleton x, empty), "Lam x e" would return Just (singleton x, singleton e), and "App e1 e2" would return Nothing.
    binder :: Traversable f => a -> Maybe ((f Name), f a)
    
    -- "Var x" = Use x, "Lam x e@(App y x)" = [Create x, Constant e, Destroy x], "App y x" = [Constant y, Constant x], "Lam x (Var x)" = [Create x, Constant (Var x), Destroy x]
    -- putting it together and flattening we get
    -- "Lam x (Lam y (App (Var x) (Var y)))" = [Create x, Constant ((Lam y (App x y)), Destroy y] = [Create x, Create y, Constant (App x y), Destroy y, Destroy x]
    -- [Create x, Create y, Constant x, Constant y, Destroy y, Destroy x] and so on
    bindSequences :: Traversable f => a -> f (Token a)

  -- Capture-avoiding substitution.
  substitute :: forall f a b. (Traversable f, Nominal f a, Uniplate a) => Name -> a -> a -> a
  substitute name for term = rewrite update term where
    -- the support of the type we're updating
    names = support term
    namesInNew = support for
    freshened = freshen names term  
    --if the name matches in a term, substitute it
    update :: a -> Maybe a
    update element = undefined where 
      terms = bindSequences element
      
  --Freshen all the variables with respect to the support of another like what in unbound
  freshen :: (Traversable f, Nominal f a) => f Name -> a -> a
  
      

  isUsage :: (Traversable f, Nominal f a) => f (Token a) -> Bool
  isUsage a = (fst res)  == 1 where
    res = mapAccumL tester 0 a
    tester 0 (Use _) = (1, 0)
    tester 1 _ = (-1, 0)
  -- Generates a fresh name with respect to a collection of names.
  gensym :: (Functor f, Foldable f) => T.Text -> Maybe Name -> Maybe NameType -> f Name -> Name
  gensym friendly uni nType xs = Name friendly new uni nType where
    new = succ . maximum . (fmap ident) $ xs
  
  -- Generates a collection of fresh names with respect to a collection of names.
  gensyms :: (Functor f, Foldable f) => T.Text -> Maybe Name -> Maybe NameType -> f Name -> Natural -> f Name
  gensyms friendly uni nType xs number = Name friendly new uni nType where
    new = succ . maximum . (fmap ident) $ xs  gensyms
  --Generates a fresh name with respect to an element.
  fresh :: (Traversable f, Nominal f a) => a -> Name
  fresh = fresh' T.empty Nothing Nothing

  -- Generates a fresh name with respect to an elemnt, with the specified properties.
  fresh' :: (Traversable f, Nominal f a) => T.Text -> Maybe Name -> Maybe NameType -> a -> Name
  fresh' friendly universe nType  = (gensym friendly universe nType) . support
-}  
  --A permutation maps names to other names.

{-
  type RawSequence a = Seq (Token a)
  data Freshness = Created
                   | Free
                   | Destructed deriving Eq
                     
  freshnessArities :: RawSequence a -> Name -> Seq Freshness
  freshnessArities a name = normaliseFreshnessArities $  assignFreshnessArities a name

  normaliseFreshnessArities ::  Seq Freshness -> Seq Freshness
  normaliseFreshnessArities (viewl -> EmptyL) = Data.Sequence.empty
  normaliseFreshnessArities (viewl -> a :< xs) = case viewl xs of
                                                  EmptyL -> Data.Sequence.singleton a
                                                  b :< ys -> case (a,b) of
                                                              (Free, Free) -> Free <| normaliseFreshnessArities ys
                                                              (Free, Destructed) -> Destructed <| normaliseFreshnessArities ys
                                                              (Created, Free) -> Created <| normaliseFreshnessArities ys
                                                              (Created, Destructed) -> normaliseFreshnessArities ys
                                                              _ -> a <| (normaliseFreshnessArities (b <| ys))
  
  assignFreshnessArities :: RawSequence a -> Name -> Seq Freshness
  assignFreshnessArities (viewl -> EmptyL) _ = Data.Sequence.empty
  assignFreshnessArities (viewl -> (x :< xs)) a = case assignArity x of
                                     Just arity -> arity <| assignFreshnessArities xs a
                                     Nothing -> assignFreshnessArities xs a
                                     where
    assignArity :: Token a -> Maybe Freshness
    assignArity (Create  n) | n == a = Just Created
    assignArity (Use     n) | n == a = Just Free
    assignArity (Destroy n) | n == a = Just Destructed
    assignArity _ = Nothing
-}
