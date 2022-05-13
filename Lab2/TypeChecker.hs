module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env

import Control.Monad

-- Método principal
typecheck :: Program -> Err ()
typecheck (PDefs defs) = do  
                 env <- populateSigma emptyEnv defs
                 mapM_ (checkDef env) defs

populateSigma :: Env -> [Def] -> Err Env
populateSigma env defs = foldM (\e (DFun returnType name args _) -> updateFun e name ((map (\(ADecl t _) -> t) args), returnType)) env defs

-- Métodos secundarios
-- Pueden agregar más método y modificar firmas de los ya declarados

inferExp :: Env -> Exp -> Err Type
inferExp env (ETrue) = return Type_bool
inferExp env (EFalse) = return Type_bool

--llamar a inferexp y ver si lo que retorna coincide con el tipo que me pasan por parametro
checkExp :: Env -> Exp -> Type -> Err ()
checkExp e exp t = do
                    t2 <- inferExp e exp 
                    if(t == t2) then return () else fail("non matching types")

--recorre la lista de stmts y aplica checkStm. Tener cuidado con las actualizaciones del context
checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms t e [] = return (e) 
checkStms t e (x:xs) = do 
                    env2 <- checkStm t e x
                    checkStms t env2 xs

--el parametro de tipo me sirve para el caso return 
checkStm :: Type -> Env -> Stm -> Err Env
checkStm t e (SExp exp) = do 
                    _ <- inferExp e exp
                    return e
checkStm t e (SDecls t2 ids) = foldM (updateVar2 t2) e ids  
checkStm t e (SInit t2 id exp) = do
                                    checkExp e exp t2
                                    updateVar e id t2
checkStm t e (SWhile exp stm) = do
                                checkExp e exp Type_bool
                                checkStm t e stm
checkStm t e (SBlock stms) = checkStms t (newBlock e) stms

--preguntar primer argumento de checkStmts
checkDef :: Env -> Def -> Err ()
checkDef (sigma, gammas) (DFun returnType id args stms) = do
                                                    env <- foldM (\e (ADecl t id2) -> updateVar e id2 t) (sigma, gammas) args
                                                    checkStms returnType env stms 
                                                    return ()