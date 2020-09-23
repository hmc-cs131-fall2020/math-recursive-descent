{-|
Module       : MathTokenizer
Description  : Tokenizer for the Math language
-}

module MathTokenizer where

import Data.Char

-- | Tokens for the Math language
data Token = CrossToken           -- ^ the '+' character
           | DashToken            -- ^ the '-' character
           | StarToken            -- ^ the '*' character
           | SlashToken           -- ^ the '/' character
           | LParenToken          -- ^ the '(' character
           | RParenToken          -- ^ the ')' character
           | NumberToken Double   -- ^ a numeric literal
  deriving (Eq, Show)

-- | The tokenize function turns a program into a list of tokens
tokenize :: String -> [Token]

tokenize [] = []

-- Tokenize literal characters
tokenize ('+':rest) = CrossToken : tokenize rest
tokenize ('-':rest) = DashToken : tokenize rest
tokenize ('*':rest) = StarToken : tokenize rest
tokenize ('/':rest) = SlashToken : tokenize rest
tokenize ('(':rest) = LParenToken : tokenize rest
tokenize (')':rest) = RParenToken : tokenize rest

-- Tokenize a sequence of characters
tokenize s@(c:rest)

  -- Tokenize a numeric literal
  | isNumber c = 
      let (num, s') = span isNumber s in
        (NumberToken (read num)) : tokenize s'

  -- Skip whitespace
  | isSpace c = tokenize rest

  -- Everything else is an error
  | otherwise = error ("Invalid character: " ++ (show c))
