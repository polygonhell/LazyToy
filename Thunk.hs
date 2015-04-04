module Thunk (i64, i64ptr, null64ptr) where

import DefnState

import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Constant as C


-- TODO these are defined in layer LLVM General releases
ptr :: Type -> Type
ptr t = PointerType t $ AddrSpace 0

i64 :: Type
i64 = IntegerType 64

i64ptr :: Type 
i64ptr = ptr i64

null64ptr :: C.Constant
null64ptr = C.Null (PointerType (IntegerType 64) $ AddrSpace 0)



data Thunk = UnevaluatedThunk {entryPoint :: String}


	
--
-- No atempt to be optimal here or even efficient
-- Just tryingt o get somethign working
-- a Thunk consists of the folowing
--  flags (64) - indicate if the alue is a value or a pointer to code to evaluate the value
--  value/codeptr/valueptr (64) - 
--

pushThunk :: Thunk -> DefnState ()
pushThunk (UnevaluatedThunk ep) = do
	return ()
