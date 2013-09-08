module MiniCore.Transforms (
    transform
) where

import MiniCore.Types
import MiniCore.Transforms.Constructors
import MiniCore.Transforms.Lambdas
import MiniCore.Transforms.BinOps

-- Program to Program transformations
-- Returns list of constructor names for printing
-- and simplified program
transform :: Program -> Stage ([Name], Program)
transform program = do
    noBinOps <- removeBinOps program
    (cons, noCons) <- transformConstructors noBinOps
    noLambdas <- liftLambdas noCons
    return (cons, noLambdas)

