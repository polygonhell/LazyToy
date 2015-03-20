{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen (genCode) where 

import Debug.Trace

import Parser
import Text.Parsec.Error

-- import Control.Monad.State
import Control.Applicative
import Control.Monad.Error

import LLVM.General.Module
import LLVM.General.Context



import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C


-- test :: ()
test = AST.defaultModule {moduleName = "Hello", moduleDefinitions = [defn]} where
  defn =   
    GlobalDefinition $ functionDefaults {
      name        = Name "poop"
    , parameters  = ([Parameter (IntegerType 32) (Name "a") []], False)
    , returnType  = IntegerType 32
    , basicBlocks = [block] 
    }
  block = BasicBlock (UnName 1) [blocks] term 
  blocks = (Name "aa") := Add False False (LocalReference $ Name "a") (ConstantOperand $ C.Int 32 7) []
  term = Do $ Ret (Just (LocalReference $ Name "aa")) []


genCodeForPrgPos :: PrgPos -> String
genCodeForPrgPos (PrgTLDef fn args expr, pos) = 
  "define i64 @" ++ fn ++ "()" ++ "{" ++ "ret i64 5" ++ "}"


liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

codegen :: AST.Module -> IO String
codegen mod = withContext $ \context ->
  liftError $ withModuleFromAST context mod $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn $ "\n\n" ++ llstr ++ "\n\n"
    return llstr


genCode :: Either ParseError [PrgPos] -> IO()
genCode (Right (h:t)) = do
  code <- codegen test
  return ()

getCode _ = error "Error"


