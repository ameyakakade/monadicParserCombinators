module Parser where

-- The parser of type 'a', is a function that takes some input, and returns a
-- list of results. Empty list means the parser failed. A list of parser means
-- it can return more than one result if there is ambiguity.
type Parser a = String -> [(a, String)]

-- -- Primitive Parsers

-- The result parser is like pure value of applicative, it puts the value v in
-- a 'minimal context'. Here it means a parser that doesn't consume the input
-- and just returns itself.
result :: a -> Parser a
result v = \inp -> [(v, inp)]

-- The zero parser is like 'empty' of alternative, it always fails regardless of
-- input
zero :: Parser a
zero = \inp -> []

-- Item parser which consumes the first char of the string, or fails if its empty
item :: Parser Char
item = \inp -> case inp of
                 []     -> []
                 (x:xs) -> [(x,xs)]
