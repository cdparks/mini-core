module MiniCore.Transforms.BinOps (
    removeBinOps
) where

import MiniCore.Types

-- Build equivalent program that has no BinOp constructors
-- They're only useful for pretty-printing
removeBinOps :: Program -> Stage Program
removeBinOps = return . map walkDecl
  where
    walkDecl (Combinator name args body) = Combinator name args $ walk body
    walkDecl decl                        = decl

-- Replace all BinOps with equivalent Apps in expressions
walk :: Expr -> Expr
walk (BinOp op e1 e2)        = App (App (Var op) $ walk e1) $ walk e2
walk (App e1 e2)             = App (walk e1) $ walk e2
walk (Let rec bindings body) = Let rec (walkBindings bindings) $ walk body
walk (Case body alts)        = Case (walk body) $ walkAlts alts
walk (Lambda args body)      = Lambda args $ walk body
walk expr                    = expr

walkBindings :: [(Name, Expr)] -> [(Name, Expr)]
walkBindings bindings = zip names exprs' where
    (names, exprs) = unzip bindings
    exprs' = map walk exprs

walkAlts :: [Alt] -> [Alt]
walkAlts alts = map walkAlt alts where
    walkAlt (PCon constructor, args, expr) = (PCon constructor, args, walk expr)

