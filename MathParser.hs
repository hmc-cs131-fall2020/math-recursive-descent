{-|
Module       : MathParser
Description  : Parser for the Math language

Contributes a parser for the following grammar:

   expr ::= factor + expr
         |  factor - expr
         |  factor * expr
         |  factor / expr
         |  factor

   factor ::= n
           | (expr)
-}

module MathParser where

import MathTokenizer

parse :: String -> Bool
parse s = 
  case expr (tokenize s) of
    (True, []) -> True            -- a successful parse that consumes all the tokens succeeds
    (True, extraTokens) -> False  -- a successful parse with leftover tokens fails
    (False, _) -> False           -- an unsuccessful parse fails


-- | A parser function takes a list of tokens and returns a pair:
--   the result of parsing and the unconsumed tokens
type ParserFunction = [Token] -> (Bool, [Token])


expr :: ParserFunction
expr tokens = 
  case factor tokens of

    -- factor + expr
    (True, CrossToken : tokens') -> expr tokens'

    -- factor - expr
    (True, DashToken : tokens') -> expr tokens'

    -- factor * expr
    (True, StarToken : tokens') -> expr tokens'

    -- factor / expr
    (True, SlashToken : tokens') -> expr tokens'

    -- factor
    (True, tokens') -> (True, tokens')

    -- If we can't parse a factor, then parsing an expr fails
    (False, tokens') -> (False, tokens')  


factor :: ParserFunction
-- n 
factor ( (NumberToken _) : tokens') = (True, tokens')

-- (expr)
factor (LParenToken : tokens') = 
  case expr tokens' of
    (True, RParenToken : tokens'') -> (True, tokens'')
    (_, tokens'') -> (False, tokens'')

-- Everything else fails
factor tokens = (False, tokens)
