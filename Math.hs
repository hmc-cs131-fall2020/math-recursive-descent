module Math where

import Parser
import MathEvaluator

run :: String -> Value
run = eval . parse
