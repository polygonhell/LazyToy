module Compiler (factorArgs, LabelGen(..)) where

import Parser 

import Control.Monad.State.Lazy
-- import Debug.Trace


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
reWriteArg :: PrgPos -> LabelState ([Bind], PrgPos)
reWriteArg (PrgApp fn args, pos) = do
  label <- newLabel
  (bindings, prgs) <- reWriteArgs args ([],[])
  let bindingPrg = case bindings of 
                      [] -> (PrgApp fn args, pos)
                      _ -> (PrgLet bindings (PrgApp fn prgs, pos), pos)
  return ([(label, bindingPrg)], (PrgId label, pos)) 
reWriteArg e = do
  return ([], e)

reWriteArgs :: [PrgPos] -> ([Bind],[PrgPos]) -> LabelState ([Bind],[PrgPos])
reWriteArgs [] res = do
  return res
reWriteArgs (h:t) (pb, pp) = do
  (binds, expr) <- reWriteArg h
  (tailBinds, tailExprs) <- reWriteArgs t (pb ++ binds, pp ++ [expr])
  return (tailBinds, tailExprs)

reWriteExpr :: PrgPos -> LabelState PrgPos
reWriteExpr (PrgApp fn args, pos) = do
  (bindings, prgs) <- reWriteArgs args ([],[])
  return (PrgLet bindings (PrgApp fn  prgs, pos), pos)
reWriteExpr e = do
  return e





factorArgs :: PrgPos -> LabelState PrgPos
factorArgs (PrgTLDef fn args expr, pos) =  do
  (b, p) <- reWriteExpr expr
  return (PrgTLDef fn args (b, p), pos)

factorArgs e = error ("Error " ++ (show e))

  












