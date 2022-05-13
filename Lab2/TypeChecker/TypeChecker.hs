module TypeChecker where

import Control.Monad
import PrintCPP

import ErrM
import AbsCPP
import Env

-- Performs the type checking to the input program
typeCheck :: Program -> Err Program
typeCheck (PDefs p) = fail "Not yet implemented"
