module Compiler (rewrite, LabelGen(..)) where

import Parser 

import Control.Monad.State.Lazy
import Debug.Trace


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

reWriteArg :: PrgPos -> LabelState ([Bind], PrgPos)
reWriteArg (PrgApp fn args, pos) = do
  label <- newLabel
  (bindings, prgs) <- reWriteArgs args ([],[])
  -- trace ("\nrewrittenArgs " ++ (show bindings) ++ "\n" ++ (show prgs) ++ "\n") 
  let bindingPrg = case bindings of 
                      [] -> (PrgApp fn args, pos)
                      _ -> (PrgLet bindings (PrgApp fn prgs, pos), pos)
  return ([(label, bindingPrg)], (PrgId label, pos)) 
  -- return ([(label, (PrgLet bindings (PrgApp fn prgs, pos), pos))], (PrgId label, pos))
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
  trace ("\nrewrittenExpr " ++ (show bindings) ++ "\n" ++ (show prgs) ++ "\n") 
    return (PrgLet bindings (PrgApp fn  prgs, pos), pos)
reWriteExpr e = do
  return e




-- reWriteExpr ((PrgApp fn args, pos) : t) (pb, pp) = do


factorArgs :: PrgPos -> LabelState PrgPos
factorArgs (PrgTLDef fn args expr, pos) =  do
  (b, p) <- reWriteExpr expr
  trace ("\n\nTest " ++ (show b) ++ "\n\n" ++ (show p) ++ "\n\n") 
    return (PrgInt 7, pos)

factorArgs e = error ("Error " ++ (show e))

  




-- compileTLDef :: PrgPos -> Either ParseError FnDef
-- compileTLDef (PrgTLDef name args expr, _) = Right(FnDef name (length args) (show expr))
-- compileTLDef e = error $ "Expected top level definition got " ++ (show e) 








