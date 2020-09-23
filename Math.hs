module Math where

import MathParser
import MathEvaluator

run :: String -> Value
run = eval . parse
