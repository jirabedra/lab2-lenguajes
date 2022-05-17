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

inferOperandTypes :: Env -> Exp -> Exp -> [Type] -> Err Type
inferOperandTypes env e1 e2 types = do
                                        t1 <- inferExp env e1
                                        t2 <- inferExp env e2
                                        if(t1==t2  && (elem t1 types)) then return t1 else fail ("mismatching types")

-- Métodos secundarios
-- Pueden agregar más método y modificar firmas de los ya declarados

inferExp :: Env -> Exp -> Err Type
inferExp env (ETrue) = return Type_bool
inferExp env (EFalse) = return Type_bool
inferExp env (EInt int) = return Type_int
inferExp env (EDouble double) = return Type_double
inferExp env (EString string) = return Type_string
inferExp env (EId id) = lookupVar env id
inferExp env (EApp id exps) = do
                            (args, ret) <- lookupFun env id
                            types <- mapM (inferExp env ) exps
                            if(types == args) then return ret else fail ("mismatched types")
inferExp env (EPIncr exp) = if((inferExp env exp) == return Type_int) then return Type_int else (if(inferExp env exp) == return Type_double then return Type_double else fail ("should be int or double"))
inferExp env (EPDecr exp) = if((inferExp env exp) == return Type_int) then return Type_int else (if(inferExp env exp) == return Type_double then return Type_double else fail ("should be int or double"))
inferExp env (EIncr exp) = if((inferExp env exp) == return Type_int) then return Type_int else (if(inferExp env exp) == return Type_double then return Type_double else fail ("should be int or double"))
inferExp env (EDecr exp) = if((inferExp env exp) == return Type_int) then return Type_int else (if(inferExp env exp) == return Type_double then return Type_double else fail ("should be int or double"))
inferExp env (ETimes exp1 exp2) = inferOperandTypes env exp1 exp2 [Type_double, Type_int]
inferExp env (EDiv exp1 exp2) = inferOperandTypes env exp1 exp2 [Type_double, Type_int]
inferExp env (EPlus exp1 exp2) = inferOperandTypes env exp1 exp2 [Type_double, Type_int, Type_string]
inferExp env (EMinus exp1 exp2) = inferOperandTypes env exp1 exp2 [Type_double, Type_int]
inferExp env (ELt exp1 exp2) = do
                                inferOperandTypes env exp1 exp2 [Type_double, Type_int]
                                return Type_bool
inferExp env (EGt exp1 exp2) = do
                                inferOperandTypes env exp1 exp2 [Type_double, Type_int]
                                return Type_bool
inferExp env (ELtEq exp1 exp2) = do
                                inferOperandTypes env exp1 exp2 [Type_double, Type_int]
                                return Type_bool
inferExp env (EGtEq exp1 exp2) = do
                                inferOperandTypes env exp1 exp2 [Type_double, Type_int]
                                return Type_bool
inferExp env (EEq exp1 exp2) = do
                                inferOperandTypes env exp1 exp2 [Type_double, Type_int, Type_bool]
                                return Type_bool
inferExp env (ENEq exp1 exp2) = do
                                inferOperandTypes env exp1 exp2 [Type_double, Type_int, Type_bool]
                                return Type_bool
inferExp env (EAnd exp1 exp2) = do
                                inferOperandTypes env exp1 exp2 [Type_bool]
                                return Type_bool
inferExp env (EOr exp1 exp2) = do
                                inferOperandTypes env exp1 exp2 [Type_bool]
                                return Type_bool
inferExp env (EAss exp1 exp2) = do
                                t1 <- inferExp env exp1
                                checkExp env exp2 t1
                                return t1



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
checkStm t e (SReturn exp) =  do 
                            t2 <- inferExp e exp
                            if(t == t2) then return (e) else fail ("mismatched types")

checkStm t e (SReturnVoid) = if(t == Type_void) then return (e) else fail ("mismatched types")
 

checkStm t e (SWhile exp stm) = do
                                checkExp e exp Type_bool
                                checkStm t e stm
                                return e
checkStm t e (SBlock stms) = do 
                            checkStms t (newBlock e) stms
                            return e

checkStm t e (SIfElse exp stm1 stm2) = do 
                                    checkExp e exp Type_bool
                                    e <- checkStm t e stm1
                                    e <- checkStm t e stm2
                                    return (e)

--preguntar primer argumento de checkStmts
checkDef :: Env -> Def -> Err ()
checkDef (sigma, gammas) (DFun returnType id args stms) = do
                                                    env <- foldM (\e (ADecl t id2) -> updateVar e id2 t) (sigma, gammas) args
                                                    checkStms returnType env stms 
                                                    return ()