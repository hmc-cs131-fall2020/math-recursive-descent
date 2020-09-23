{-|
Module       : MathEvaluator
Description  : Evaluator for the Math language
-}

module MathEvaluator where

import MathAST

-- | The evaluator results in a Double
type Value = Double

-- | Evaluate an expression, with an environment
eval :: Exp -> Value

-- Numeric literals
eval (Num n) = n

-- Binary operations
eval (BinOp left PlusOp  right) = eval left + eval right
eval (BinOp left MinusOp right) = eval left - eval right
eval (BinOp left TimesOp right) = eval left * eval right
eval (BinOp left DivOp   right) = eval left / eval right
