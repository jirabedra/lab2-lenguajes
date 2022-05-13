-- Defines the environment operations needed for the typechecker
module Env(Env, lookupVar, lookupFunc, updateVar, updateFunc, newBlock, emptyEnv) where

import AbsCPP
import ErrM
import Data.List

-- Here, define the type of the Environment
-- String is only here as placeholder
type Env = String

-- Given the Environment and a name of a variable
-- retrieves the type of the said variable, if it exists
lookupVar :: Env -> Id -> Err Type
lookupVar env id = fail "Not yet implemented"

-- Given the Environment and a name of a function
-- retrieves the type of its arguments and the return type, if the function was declared
lookupFunc :: Env -> Id ->  Err ([Type], Type)
lookupFunc env id = fail "Not yet implemented"

-- Given the Environment, insert the type of the variable with the given name
-- Fails if the variable has already been defined in the scope
updateVar :: Env -> Id -> Type -> Err Env
updateVar env id typ = fail "Not yet implemented"

-- Given the Environment, insert the function signature with the types of its arguments
-- and the return type
-- Fails if the function has already been defined
updateFunc :: Env -> Id -> ([Type], Type) -> Err Env
updateFunc env id sig = fail "Not yet implemented"

-- Given the Environment, defines a new scope to be used
-- Variables defined in this scope are allowed to live only inside it
-- Variables defined in outer scopes can be redefined within this new scope
newBlock :: Env -> Err Env
newBlock env = fail "Not yet implemented"

-- Creates an empty Environment
-- To be called at the begining of the type checking
emptyEnv :: Env
emptyEnv = undefined
