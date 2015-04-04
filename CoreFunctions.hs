module CoreFunctions (coreDefs) where

import DefnState
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

import Control.Monad.State




tStackPtr :: Definition
tStackPtr = GlobalDefinition $ globalVariableDefaults {
    G.name = Name "__tStack"
  , G.type' = i64ptr
  , G.initializer = Just null64ptr
}


entryPoint :: DefnState ()
entryPoint = do
  -- Init the stack pointer
  s <- call (globalFn "malloc") [int64 1000000]
  store (global (Name "__tStack")) s
  retVal <- call (globalFn "main") []
  ret retVal
  endBlock
  return ()


pushThunk :: Definition
pushThunk = GlobalDefinition $ functionDefaults {
    G.name = Name "__pushThunk"
  , G.returnType = i64ptr
  }


coreFnDefs :: [(String, [String], DefnState ())]
coreFnDefs = [("__entryPoint", [], entryPoint)]

fnDefs :: [Definition]
fnDefs   = map fnDef coreFnDefs


externalFn ::  Type -> String -> [(Type, String)] -> Definition
externalFn retT name args = GlobalDefinition $ functionDefaults {
    G.name = Name name
  , G.returnType = retT
  , G.parameters  = (params, False)
  } where
    params = [Parameter t (Name s) [] | (t, s) <- args]



fnDef :: (String, [String], DefnState a) -> Definition
fnDef (name, args, e) = 
  let defn = defaultDefn {fnName = name, fnArgs = a}
      a = [(i64ptr, Name n) | n <- args]
    in
    makeDefn $ execState e defn 




coreDefs :: [Definition]
coreDefs = [
    tStackPtr
  , externalFn i64ptr "malloc" [(i64, "size")]
  ] ++ fnDefs

