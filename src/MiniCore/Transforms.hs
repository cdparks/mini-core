module MiniCore.Transforms (
    transform
) where

import MiniCore.Types
import MiniCore.Transforms.Constructors
import MiniCore.Transforms.Lambdas
import MiniCore.Transforms.BinOps

-- Program to Program transformations
transform :: Program -> Stage Program
transform program = liftLambdas =<< transformConstructors =<< removeBinOps program

