module Infer (

) where

import Parse (Expr)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Control.Monad.Except
import qualified GHC.List as List
import Eval (toString)
import Data.Char (chr)

-- basic datatypes. funcs curry for arity > 1
data Type = Var String
        | Int
        | Bool
        | Func Type Type
    deriving (Eq, Ord)

-- a scheme for quantified types: \forall a, b. a -> b -> a will have:
-- Scheme ["a", "b"] [...]
data Scheme = Scheme [String] Type

-- type substitutions: from strings to actual type variables
type Subst = Map.Map String Type


-- ftv gets the free variables in `a` (ie ones that aren't bound by quantifiers)
-- apply takes a substitution and a type, and makes all substitutions found in it
class Types a where
    ftv :: a -> Set.Set String
    apply :: Subst -> a -> a

instance Types Type where

    -- free type vars in a type are simply all types, since there is no quantification
    ftv (Var a) = Set.singleton a
    ftv Int = Set.empty
    ftv Bool = Set.empty
    ftv (Func a b) = Set.union (ftv a) (ftv b)
    -- apply is simple here too 
    apply s (Var a) = fromMaybe (Var a) (Map.lookup a s)
    apply s (Func a b) = Func (apply s a) (apply s b)
    apply _ a = a

instance Types Scheme where
    ftv (Scheme vars a) = Set.difference (ftv a) (Set.fromList vars)

    apply s (Scheme vars a) = Scheme vars (apply (foldr Map.delete s vars) a)

instance Types a => Types [a] where
    ftv [a] = List.foldr (Set.union . ftv) Set.empty [a]
    apply s [a] = List.map (apply s) [a]



emptySubst :: Subst
emptySubst = Map.empty

newtype TypeEnv = TypeEnv (Map.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme (Set.toList $ Set.difference (ftv t) (ftv env)) t


type TI a = ExceptT String (State Int) a

indexToLetters :: Int -> String
indexToLetters n
  | n < 0     = error "Index must be non-negative"
  | otherwise = reverse (go (n + 1))
  where
    go 0 = ""
    go x = let (q, r) = (x - 1) `divMod` 26
           in chr (65 + r) : go q

newTypeVar :: TI Type
newTypeVar = do
    s <- get
    put (s + 1)
    return (Var (indexToLetters s))


instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (const newTypeVar) vars
    let s = Map.fromList (zip vars nvars)
    return (apply s t)
