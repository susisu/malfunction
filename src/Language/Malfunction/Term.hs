module Language.Malfunction.Term where

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
