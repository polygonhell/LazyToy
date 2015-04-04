module DefnState  where


import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CC


import Data.Word

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
