module Compiler (rewrite, LabelGen(..)) where

import Parser 

import Control.Monad.State.Lazy
import Debug.Trace

type Binding = (String, PrgPos)

data FnDef = FnDef String Int String
  deriving (Eq, Show)

type LabelState
 = State LabelGen

data LabelGen =
  LabelGen { count :: Integer }

newLabel :: LabelState String
newLabel = do
  st <- get
  put $ st {count = (count st) + 1}
  return $ "__" ++ (show $ count st)


-- Factor outsub expressions
-- factorApp :: FactorState ([Binding], PrgPos)

reWriteArgs :: [PrgPos] -> ([Binding],[PrgPos]) -> LabelState ([Binding],[PrgPos])

reWriteArgs [] res = do
  return res
reWriteArgs ((PrgApp fn args, pos) : t) (pb, pp) = do
  label <- newLabel
  return ([(label, (PrgApp fn args, pos))], [(PrgApp fn args, pos)])
  

-- reWriteExpr ((PrgApp fn args, pos) : t) (pb, pp) = do


rewrite :: PrgPos -> LabelState PrgPos
rewrite (PrgTLDef fn args expr, pos) = do
  traceM $ ("\n\nHere\n\n ")
  (b, p) <- reWriteArgs [expr] ([],[])
  traceM $ ("\n\nTest " ++ (show b))
  return (PrgInt 7, pos)
rewrite e = error ("Error " ++ (show e))

  




-- compileTLDef :: PrgPos -> Either ParseError FnDef
-- compileTLDef (PrgTLDef name args expr, _) = Right(FnDef name (length args) (show expr))
-- compileTLDef e = error $ "Expected top level definition got " ++ (show e) 








