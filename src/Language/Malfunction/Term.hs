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
