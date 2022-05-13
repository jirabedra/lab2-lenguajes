module Env where

import AbsCPP
import PrintCPP
import ErrM

import qualified Data.Map.Strict as Map

-- Pueden hacer uso de las funciones de Map

-- Map es una lista de pares

-- Definición de un Environment
type Env = (Sig,[Context])
type Sig = Map.Map Id ([Type],Type)
type Context = Map.Map Id Type


lookupVar :: Env -> Id -> Err Type
lookupVar (sigma, context) id = case context of { 
    [] -> fail("Id not found");
    x:xs -> case (Map.lookup id x) of { 
        Nothing -> lookupVar (sigma, xs) id;
        Just someType -> return (someType);
    }
}

lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (sigma, gammas) id = case (Map.lookup id sigma) of {
    Nothing -> fail("Id not found");    
    Just (someArgs, returnTypes) -> return ((someArgs, returnTypes)); 
}

--update en el sentido de agregar al contexto
updateVar :: Env -> Id -> Type -> Err Env
updateVar e id t = case e of {
    (a,b) -> if (Map.member id (head b))
            then fail ("Id already exists")
            else return (a, (Map.union (head b) (Map.singleton id t)):(tail b))
}

updateVar2 :: Type -> Env -> Id -> Err Env
updateVar2 t e id = case e of {
    (a,b) -> if (Map.member id (head b))
            then fail ("Id already exists")
            else return (a, (Map.union (head b) (Map.singleton id t)):(tail b))
}

--consultar
updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun e id par = case e of {
    (a,b) -> if (Map.member id a)
            then fail("Id already exists")
            else return ((Map.union a (Map.singleton id par),b))
}

newBlock :: Env -> Env
newBlock (sigma, gammas)= (sigma, (Map.empty):gammas)

emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])
--o podria ser lista con map.empty
