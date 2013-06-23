module MiniCore.Transforms (
    transform
) where

import MiniCore.Types
import MiniCore.Transforms.Constructors
--import MiniCore.Transforms.Lambdas

-- Program to Program transformations
transform :: Program -> Program
--transform = liftLambdas . liftConstructors
transform = fst . convertConstructors

