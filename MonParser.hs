module Parser where
import Data.Char
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> [(a, String)] }
  deriving (Functor)

-- Can change these 3 later to redefine parsers
result :: a -> Parser a
result v = Parser $ \inp -> [(v,inp)]

zero :: Parser a
zero = Parser $ \inp -> []

item :: Parser Char
item = Parser $ \inp -> case inp of
                 []     -> []
                 (x:xs) -> [(x,xs)]

-- Combination or juxtaposition operator
bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = Parser $ \inp -> concat [runParser (f v) inp' | (v, inp') <- runParser p inp]

p `seq` q = p `bind` \x ->
  q `bind` \y ->
  result (x, y)

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then result x else zero

char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (isDigit)

lower :: Parser Char
lower = sat (isLower)

upper :: Parser Char
upper = sat (isUpper)

-- Choice operator
plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ \inp -> (runParser p inp ++ runParser q inp)

letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

word :: Parser String
word = neWord `plus` result ""
  where
    neWord = do
      x <- letter
      xs <- word
      result (x:xs)

string :: String -> Parser String
string "" = result ""
string (x:xs) = do
    char x
    string xs
    return (x:xs)

instance Applicative Parser where
    pure b = Parser $ \inp -> [(b, inp)]
    (<*>) a b = do
        f <- a
        i <- b
        return (f i)

instance Monad Parser where
    (>>=) p f = Parser $ \inp -> concat [ runParser (f v) out | (v,out) <- runParser p inp]

many' :: Parser a -> Parser [a]
many' p = (do
    x <- p
    xs <- many' p
    return (x:xs)) `plus` return []

ident :: Parser String
ident = do
    x  <- lower
    xs <- many' alphanum
    return (x:xs)

many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many' p
    return (x:xs)

nat :: Parser Int
nat = fmap read (many1 digit)

int :: Parser Int
int = do
    f <- op
    n <- nat
    return (f n)
      where op = fmap (const negate) (char '-') `plus` return id

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = do
    x <- p
    xs <- many' (sep *> p)
    return (x:xs)

ints :: Parser [Int]
ints = char '[' *> sepby1 int (char ',') <* char ']'

---------------------------------

type LParser a = String -> Maybe (a, String)

lresult :: a -> LParser a
lresult v = \inp -> Just (v,inp)

lzero :: LParser a
lzero = \inp -> Nothing

litem :: LParser Char
litem = \inp -> case inp of
                 []     -> Nothing
                 (x:xs) -> Just (x,xs)
