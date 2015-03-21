{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen (genCode) where 

import Debug.Trace

import Parser
import Text.Parsec.Error


import LLVM.General.Module 
import LLVM.General.Context



import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Analysis

import Data.Word

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State

data Defn = Defn
  { nextLabel :: Word
  , fnName :: String
  , instructions :: [Named Instruction]
  , term :: Maybe (Named Terminator)
  , blocks :: [BasicBlock]
  } deriving (Eq, Show)

defaultDefn :: Defn
defaultDefn = Defn 0 "" [] Nothing []

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
  modify $ \s -> s {instructions = [], term = Nothing, blocks = blks ++ [newBlock]}
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

add :: Operand -> Operand -> DefnState Operand
add a b = do
  instruction $ Add False False a b []

ret :: Operand -> DefnState (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []



test2 :: DefnState ()
test2 = do
  b1 <- add (local (Name "a")) (int 5)
  b2 <- add b1 (int 3)
  ret b2
  endBlock
  return ()


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


-- Note later versions of LLVM return a ExceptT rather than an ErrorT 
liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

applyToAST :: AST.Module -> (LLVM.General.Module.Module -> IO a) -> IO a
applyToAST mod fn = withContext $ \context ->
    liftError $ withModuleFromAST context mod $ \m -> do
      fn m

passes::PassSetSpec
passes=defaultCuratedPassSetSpec{optLevel=Just 2}

codegen :: AST.Module -> IO AST.Module 
codegen mod = applyToAST mod $ \m -> do
  withPassManager passes $ \pm -> do
    runPassManager pm m
    moduleAST m




makeDefn :: Defn -> Definition
makeDefn def = GlobalDefinition $ functionDefaults {
    name        = Name $ fnName def
  , parameters  = ([Parameter (IntegerType 32) (Name "a") []], False)
  , returnType  = IntegerType 32
  , basicBlocks = blocks def
  } 


makeModule :: String -> [Definition] -> AST.Module
makeModule name defs = AST.defaultModule { moduleName = name, moduleDefinitions = defs}




genCode :: Either ParseError [PrgPos] -> IO()
genCode (Right (h:t)) = do
  putStrLn $ "\ntest2\n" ++ (show mod) ++ "\n\n"
  code <- codegen mod
  putStrLn $ "\ntest2\n" ++ (show code) ++ "\n\n"
  applyToAST code $ \m -> do 
    str <- moduleLLVMAssembly m
    putStrLn $ "\nOp\n" ++ str ++ "\n\n"
  where
    defn = makeDefn $ execState test2 $ defaultDefn {fnName = "main"}
    mod = makeModule "main" [defn]




genCode _ = error "Error"


