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

import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Analysis


test = AST.defaultModule {moduleName = "Hello", moduleDefinitions = [defn]} where
  defn =   
    GlobalDefinition $ functionDefaults {
      name        = Name "poop"
    , parameters  = ([Parameter (IntegerType 32) (Name "a") []], False)
    , returnType  = IntegerType 32
    , basicBlocks = [block] 
    }
  block = BasicBlock (UnName 1) blocks term 
  blocks = 
    [ (Name "aa") := Add False False (LocalReference $ Name "a") (ConstantOperand $ C.Int 32 3) [] 
    , (Name "bb") := Sub False False (LocalReference $ Name "aa") (ConstantOperand $ C.Int 32 5) [] 
    ]
  term = Do $ Ret (Just (LocalReference $ Name "bb")) []


genCodeForPrgPos :: PrgPos -> String
genCodeForPrgPos (PrgTLDef fn args expr, pos) = 
  "define i64 @" ++ fn ++ "()" ++ "{" ++ "ret i64 5" ++ "}"


liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return


applyToAST :: AST.Module -> (LLVM.General.Module.Module -> IO a) -> IO a
applyToAST mod fn = withContext $ \context ->
    liftError $ withModuleFromAST context mod $ \m -> do
      fn m

passes::PassSetSpec
passes=defaultCuratedPassSetSpec{optLevel=Just 3}

codegen :: AST.Module -> IO AST.Module 
codegen mod = applyToAST mod $ \m -> do
    withPassManager passes $ \pm -> do
      runPassManager pm m
      moduleAST m


genCode :: Either ParseError [PrgPos] -> IO()
genCode (Right (h:t)) = do
  code <- codegen test
  applyToAST code $ \m -> do 
      str <- moduleLLVMAssembly m
      putStrLn $ "\nOp\n" ++ str ++ "\n\n"


  return ()

getCode _ = error "Error"


