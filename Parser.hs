-- -- -- Should probably read this along with the linked paper.

module Parser where
import Data.Char
import Control.Applicative

-- The parser of type 'a', is a function that takes some input, and returns a
-- list of results. Empty list means the parser failed. A list of parser means
-- it can return more than one result if there is ambiguity.
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

-- -- Primitive Parsers

-- The result parser is like pure value of applicative, it puts the value v in
-- a 'minimal context'. Here it means a parser that doesn't consume the input
-- and just returns itself.
result :: a -> Parser a
result v = Parser $ \inp -> [(v, inp)]

-- The zero parser is like 'empty' of alternative, it always fails regardless of
-- input
zero :: Parser a
zero = Parser $ \inp -> []

-- Item parser which consumes the first char of the string, or fails if its empty
item :: Parser Char
item = Parser $ \inp -> case inp of
                          []     -> []
                          (x:xs) -> [(x,xs)]

-- -- Sequencing Parsers
-- To form useful parsers, we need a sequencing operator, and a choice operator.

-- Sequencing means apply one parser after another, and combining their results.
-- A operator which returns a tuple containing results of the two parsers was
-- used before (called `seq`), but this leads to parsers with nested tuples as
-- results. Combining them is messy. We use the `>>=` (bind) operator of monads which
-- allows us to sequence AND merge the values simultaneously.

-- We apply the first parser and get a list of possible results. We apply the
-- second parser to each of the possible results, which returns a lists of
-- lists, which we flatten with concat.

instance Monad Parser where
    p >>= f = Parser $ \input -> let resultList = runParser p input
                                 in concat $ map (\(v, restIn) -> runParser (f v) restIn) resultList

-- We can define the mentioned 'seq' operator with our new monadic bind. The
-- reverse cannot be done.
p `seq'` q = p >>= \x ->
             q >>= \y ->
             result (x,y)

-- The above is like using '<-' inside do blocks
p `seq` q = do
  x <- p
  y <- q
  result (x,y)

-- -- I will use do blocks instead of the '>>=' + lambda notation for simplicity.

-- Here is a function that takes in a condition and returns a parser that only
-- consumes if the condition is met.
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then result x else zero

-- Defining parsers using sat

-- Parsing specific character
char :: Char -> Parser Char
char x = sat (\y -> x == y)

-- Parsing single digits
digit :: Parser Char
digit = sat isDigit -- isDigit returns true if digit is provided

-- Parsing single alphabets
alpha :: Parser Char
alpha = sat isAlpha

-- This is our choice operator. We feed the same input to both parsers and
-- add their resulting lists. If a parser fails, it will be like adding [] (empty
-- list). List returned will contain all the possible 'choices'. <|> operator
-- will be used as a alias to plus.
plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ \inp -> ((runParser p inp)++(runParser q inp))

-- Example parser to parse a single alphanumeric.
alphaNum = alpha <|> digit

word :: Parser String
word = neWord <|> result ""
       where
         neWord = do
           x <- alpha
           xs <- word
           result (x:xs)

instance Alternative Parser where
    empty = zero
    p <|> q = plus p q

-- Things to make haskell happy.
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (a, restIn) <- p input
    return (f a, restIn)

-- We used the 'result' we defined above as it means the same thing as pure
instance Applicative Parser where
    pure = result
