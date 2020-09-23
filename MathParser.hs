{-|
Module       : MathParser
Description  : Parser for the Math language

Contributes a parser for the following grammar:

   expr ::= expr + expr
         |  expr - expr
         |  expr * expr
         |  expr / expr
         |  number
         |  (expr)
-}

module MathParser where

import MathTokenizer

parse :: String -> Bool
parse s = expr (tokenize s)

type ParserFunction = [Token] -> Bool

expr :: ParserFunction
expr tokens = (expr tokens && isCrossToken tokens && expr tokens)
           || (expr tokens && isDashToken tokens && expr tokens)
           || (expr tokens && isStarToken tokens && expr tokens)          
           || (expr tokens && isSlashToken tokens && expr tokens)
           || (isNumberToken tokens)
           || (isLParenToken tokens && expr tokens && isRParenToken tokens)

  where isCrossToken (CrossToken : _) = True
        isCrossToken _ = False

        isDashToken (DashToken : _) = True
        isDashToken _ = False

        isStarToken (StarToken : _) = True
        isStarToken _ = False

        isSlashToken (SlashToken : _) = True
        isSlashToken _ = False

        isNumberToken ( (NumberToken _) : _) = True
        isNumberToken _ = False

        isLParenToken (LParenToken : _) = True
        isLParenToken _ = False

        isRParenToken (RParenToken : _) = True
        isRParenToken _ = False
