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
parse s = expr (tokenize s)

type ParserFunction = [Token] -> Bool

expr :: ParserFunction
expr tokens = (factor tokens && isCrossToken tokens && expr tokens)
           || (factor tokens && isDashToken tokens && expr tokens)
           || (factor tokens && isStarToken tokens && expr tokens)          
           || (factor tokens && isSlashToken tokens && expr tokens)
           || (factor tokens)

  where isCrossToken (CrossToken : _) = True
        isCrossToken _ = False

        isDashToken (DashToken : _) = True
        isDashToken _ = False

        isStarToken (StarToken : _) = True
        isStarToken _ = False

        isSlashToken (SlashToken : _) = True
        isSlashToken _ = False


factor :: ParserFunction
factor tokens = (isNumberToken tokens)
             || (isLParenToken tokens && expr tokens && isRParenToken tokens)

  where isNumberToken ( (NumberToken _) : _) = True
        isNumberToken _ = False

        isLParenToken (LParenToken : _) = True
        isLParenToken _ = False

        isRParenToken (RParenToken : _) = True
        isRParenToken _ = False
