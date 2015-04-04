module CoreFunctions (coreDefs) where

import Thunk

-- import LLVM.General.Module 
-- import LLVM.General.Context
import LLVM.General.AST
import LLVM.General.AST.Type
import LLVM.General.AST.Global as G
import LLVM.General.AST.AddrSpace
-- import qualified LLVM.General.AST as AST
-- import qualified LLVM.General.AST.Constant as C
-- import qualified LLVM.General.AST.CallingConvention as CC



tStackPtr :: Definition
tStackPtr = GlobalDefinition $ globalVariableDefaults {
    G.name = Name "__tStack"
  , G.type' = i64ptr
  , G.initializer = Just null64ptr
}

pushThunk :: Definition
pushThunk = GlobalDefinition $ functionDefaults {
    G.name = Name "pushThunk"
  , G.returnType = i64ptr
  }

 -- pushThunk = GlobalDefinition $ functionDefaults {
 --    name        = "pushThunk"
 --  , returnType  = PointerType (IntegerType 64) $ AddrSpace 0
 --  , basicBlocks = [block] } where
 --    block = BasicBlock UnName 0

coreDefs :: [Definition]
coreDefs = [tStackPtr, pushThunk]