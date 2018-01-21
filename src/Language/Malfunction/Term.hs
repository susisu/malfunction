module Language.Malfunction.Term where

import Control.Monad (when)
import Data.List (nub, elemIndex)

-- identifier
type Id = String

data Literal = LitInt Int      -- integer
             | LitFloat Double -- float
             | LitChar Char    -- character
             | LitStr String   -- string


data Pattern = PatVar Id           -- variable
             | PatWild             -- wildcard
             | PatLit Literal      -- literal
             | PatAs Id Pattern    -- as-pattern
             | PatCon Id [Pattern] -- constructor


data Term a = TmVar a Id                        -- variable
            | TmLit a Literal                   -- literal
            | TmApp a (Term a) [Term a]         -- application
            | TmAbs a [Pattern] (Term a)        -- lambda-abstraction
            | TmLet a Pattern (Term a) (Term a) -- let-in binding
            | TmIf a (Term a) (Term a) (Term a) -- if-then-else
            | TmCase a [(Pattern, Term a)]      -- pattern matching

getInfo :: Term a -> a
getInfo (TmVar i _)     = i
getInfo (TmLit i _)     = i
getInfo (TmApp i _ _)   = i
getInfo (TmAbs i _ _)   = i
getInfo (TmLet i _ _ _) = i
getInfo (TmIf i _ _ _)  = i
getInfo (TmCase i _)    = i


type Level = Int

data Type = TyVar Level Id  -- type variable
          | TyCon Id [Type] -- type constructor
          | TyArr Type Type -- arrow (function type)
          | TyGVar Int      -- generalized type variable

data TypeScheme = TyScheme Int Type

generalize :: Level -> Type -> TypeScheme
generalize l ty = TyScheme (length gfv) (replace ty)
  where
    gFreeVars (TyVar l' v)    = if l' >= l then [v] else []
    gFreeVars (TyCon _ tys)   = nub $ concatMap gFreeVars tys
    gFreeVars (TyArr ty1 ty2) = nub $ gFreeVars ty1 ++ gFreeVars ty2
    gFreeVars (TyGVar _)      = []

    gfv = gFreeVars ty

    replace ty@(TyVar l' v) =
      case elemIndex v gfv of
        Just n  -> TyGVar n
        Nothing -> ty
    replace (TyCon c tys)   = TyCon c (map replace tys)
    replace (TyArr ty1 ty2) = TyArr (replace ty1) (replace ty2)
    replace ty@(TyGVar _)   = ty

instantiate :: Monad m => TypeScheme -> [Type] -> m Type
instantiate (TyScheme arity gty) args =
  do
    when (length args /= arity) $ fail "wrong number of arguments"
    return $ replace gty
  where
    replace ty@(TyVar _ _)  = ty
    replace (TyCon c tys)   = TyCon c (map replace tys)
    replace (TyArr ty1 ty2) = TyArr (replace ty1) (replace ty2)
    replace (TyGVar n)      = args !! n
