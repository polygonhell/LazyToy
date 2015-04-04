{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen (genCode, DefnState) where 

import Debug.Trace

import Parser
import Text.Parsec.Error
import CoreFunctions
import DefnState
import Thunk

import LLVM.General.Module 
import LLVM.General.Context

import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
-- import qualified LLVM.General.AST.CallingConvention as CC

import LLVM.General.PassManager
import LLVM.General.Target

-- import Data.Word

import Control.Monad.Error
import Control.Monad.State


-- Note later versions of LLVM return a ExceptT rather than an ErrorT 
liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

applyToAST :: AST.Module -> (LLVM.General.Module.Module -> IO a) -> IO a
applyToAST ast fn = withContext $ \context ->
    liftError $ withModuleFromAST context ast $ \m -> do
      fn m

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 0 }

optimize :: AST.Module -> IO AST.Module 
optimize ast = applyToAST ast $ \m -> do
  withPassManager passes $ \pm -> do
    runPassManager pm m
    moduleAST m




makeModule :: String -> [Definition] -> AST.Module
makeModule mName defs = AST.defaultModule { moduleName = mName, moduleDefinitions = defs ++ coreDefs}

nativeAssembly :: LLVM.General.Module.Module -> IO String
nativeAssembly ast = 
  liftError $ withDefaultTargetMachine $ \tm -> do
    liftError $ moduleTargetAssembly tm ast



-- Walk the AST and generate LLVM AST
processPrg :: PrgPos -> DefnState Operand
processPrg ((PrgTLDef name args expr), pos) = do
  r <- processPrg expr
  ret r
  endBlock
  return r

processPrg ((PrgId name), pos) = do
  return $ local $ Name name

-- TL Functiondef called
processPrg ((PrgApp ((PrgId name),_) args), pos) = do
  r <- call (globalFn name) [ConstantOperand (C.Null (PointerType (IntegerType 64) $ AddrSpace 0))]
  return r

processPrg ((PrgApp fn args), pos) = do
  return $ int 5

processPrg ((PrgLet binds expr), pos) = do
  processPrg expr



processDefn :: PrgPos -> Definition
processDefn ((PrgTLDef name args expr), pos) = 
  let defn = defaultDefn {fnName = name, fnArgs = a}
      a = [(i64ptr, Name n) | n <- args]
      e = processPrg ((PrgTLDef name args expr), pos)
    in
    makeDefn $ execState e defn 





genCode :: Either ParseError [PrgPos] -> IO()
genCode (Right (prgs)) = do
  applyToAST ast $ \m -> do 
    str <- moduleLLVMAssembly m
    putStrLn $ "\nOp\n" ++ str ++ "\n\n"
    native <- nativeAssembly m
    putStrLn $ "\nNative\n" ++ native ++ "\n\n"
  code <- optimize ast
  applyToAST code $ \m -> do 
    str <- moduleLLVMAssembly m
    putStrLn $ "\nOp\n" ++ str ++ "\n\n"
    native <- nativeAssembly m
    putStrLn $ "\nNative\n" ++ native ++ "\n\n"

  where
    defns = [processDefn d | d <-prgs]
    ast = makeModule "main" (defns)




genCode _ = error "Error"


