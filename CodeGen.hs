{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen (genCode) where 

import Debug.Trace

import Parser
import Text.Parsec.Error


import LLVM.General.Module 
import LLVM.General.Context



import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CC

import LLVM.General.PassManager
import LLVM.General.Target

import Data.Word

import Control.Monad.Error
import Control.Monad.State

data Defn = Defn
  { nextLabel :: Word
  , fnName :: String
  , fnArgs :: [(Type, Name)]
  , instructions :: [Named Instruction]
  , term :: Maybe (Named Terminator)
  , blocks :: [BasicBlock]
  } deriving (Eq, Show)

defaultDefn :: Defn
defaultDefn = Defn 0 "" [] [] Nothing []

type DefnState = State Defn 

getUnLabel :: DefnState Name
getUnLabel = do
  i <- gets nextLabel
  modify $ \s -> s {nextLabel = i+1}
  return $ UnName i

makeBlock :: Defn -> DefnState BasicBlock
makeBlock def = do 
  ref <- getUnLabel
  return $ BasicBlock ref instr (makeTerm t) 
  where 
    instr = instructions def
    t = term def
    makeTerm (Just a) = a
    makeTerm _ = error "No Terminator on block"


endBlock :: DefnState BasicBlock
endBlock = do
  s <- get
  newBlock <- makeBlock s
  let blks = blocks s
  modify $ \s2 -> s2 {instructions = [], term = Nothing, blocks = blks ++ [newBlock]}
  return newBlock 


instruction :: Instruction -> DefnState Operand
instruction inst = do
  ref <- getUnLabel
  insts <- gets instructions
  modify $ \s -> s {instructions = insts ++ [ref := inst]}
  return $ LocalReference ref

terminator :: Named Terminator -> DefnState (Named Terminator)
terminator t = do
  modify $ \s -> s {term = Just t}
  return t

int :: Integer -> Operand
int a = ConstantOperand $ C.Int 32 a

local :: Name -> Operand
local a = LocalReference $ a

global :: Name -> Operand
global a = ConstantOperand (C.GlobalReference a)

add :: Operand -> Operand -> DefnState Operand
add a b = do
  instruction $ Add False False a b []

ret :: Operand -> DefnState (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

-- Note preconfigured optimizer pass fails with CC.Fast, generates a bad tail call
-- I think this is an issue with this version of LLVM General and it's inability to pass 
-- the type of the Glabal reference that is the Callee
-- Docs state that "requires the prototype of all callees to exactly match the prototype of the function definition"
call :: CallableOperand -> [Operand] -> DefnState Operand
call fn args = do
  instruction $ Call False CC.C [] fn [(a, []) | a <- args] [] []


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


makeDefn :: Defn -> Definition
makeDefn def = GlobalDefinition $ functionDefaults {
    name        = Name $ fnName def
  , parameters  = (params, False)
  , returnType  = PointerType (IntegerType 64) $ AddrSpace 0
  , basicBlocks = blocks def
  } where 
    params = [Parameter a b [] | (a,b) <- fnArgs def]


makeModule :: String -> [Definition] -> AST.Module
makeModule mName defs = AST.defaultModule { moduleName = mName, moduleDefinitions = defs}

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
  r <- call (Right (global (Name name))) [ConstantOperand (C.Null (PointerType (IntegerType 64) $ AddrSpace 0))]
  return r

processPrg ((PrgApp fn args), pos) = do
  return $ int 5

processPrg ((PrgLet binds expr), pos) = do
  processPrg expr



processDefn :: PrgPos -> Definition
processDefn ((PrgTLDef name args expr), pos) = 
  let defn = defaultDefn {fnName = name, fnArgs = a}
      a = [(PointerType (IntegerType 64) $ AddrSpace 0, Name n) | n <- args]
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


