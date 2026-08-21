module Parser where
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy

-- Can change these 3 later to redefine parsers
result :: a -> Parser a
result v = Parser $ \inp -> [(v,inp)]

zero :: Parser a
zero = Parser $ const []

item :: Parser Char
item = Parser $ \inp -> case inp of
                 []     -> []
                 (x:xs) -> [(x,xs)]

-- Combination or juxtaposition operator
bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = Parser $ \inp -> concat [runParser (f v) inp' | (v, inp') <- runParser p inp]

p `seq` q = do
    x <- p
    y <- q
    return (x, y)

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then result x else zero

char :: Char -> Parser Char
char x = sat (x ==)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

-- Choice operator
plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ \inp -> runParser p inp ++ runParser q inp

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
many' p = force $ (do
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
    f <$> nat
      where op = fmap (const negate) (char '-') `plus` return id

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = do
    x <- p
    xs <- many' (sep *> p)
    return (x:xs)

ints :: Parser [Int]
ints = bracket (char '[') (sepby1 int (char ',')) (char ']')

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = sepby1 p sep `plus` return []

expr' :: Parser Int
expr' = term `chainl1` addop

term :: Parser Int
term = factor `chainr1` expop

factor :: Parser Int
factor = nat `plus` bracket (char '(') expr' (char ')')

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = open *> p <* close

ops :: [(Parser a, b)] -> Parser b
ops = foldr1 plus . map (\(p, f) -> f <$ p)

addop :: Parser (Int -> Int -> Int)
addop = ops [ (char '+', (+))
            , (char '-', (-))
            ]
        
expop :: Parser (Int -> Int -> Int)
expop = ops [ (char '^', (^))
            ]

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p `bind` rest
  where
    rest acc = (do
                     f <- op
                     x <- p 
                     rest (f acc x)
               ) `plus` return acc

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p `bind` rest
  where
    rest x = (do
                   f <- op
                   xs <- chainr1 p op
                   return (f x xs)
             ) `plus` return x

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v = chainr1 p op `plus` return v

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v = chainl1 p op `plus` return v

-- Parser given as input is always expected to succeed.
-- This is because 'head' function is used which will crash on a empty list
-- Why does this work ?
force :: Parser a -> Parser a
force p = Parser $ \inp -> let x = runParser p inp
                           in (fst (head x), snd (head x)) : tail x

first :: Parser a -> Parser a
first p = Parser $ \inp -> case runParser p inp of
                             []     -> []
                             (x:xs) -> [x]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p `plus` q)

spaces :: Parser ()
spaces = do
    many1 (sat isSpace)
    return ()

comment :: Parser ()
comment = do
    string "--"
    many' (sat (/='\n'))
    return ()

mlcomment :: Parser ()
mlcomment = do
    string "{-"
    many' (sat (/='-') +++ (char '-' *> sat (/= '}')))
    string "-}"
    return ()

junk :: Parser ()
junk = () <$ many' (spaces +++ comment +++ mlcomment)

parse :: Parser a -> Parser a
parse p = junk *> p

token :: Parser a -> Parser a
token p = p <* junk

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: [String] -> Parser String
identifier ks = token (
    do
        x <- ident
        if not (elem x ks)
          then return x
          else zero
    )

data Expr = App Expr Expr        -- application
          | Lam String Expr      -- lambda abstraction
          | Let String Expr Expr -- local definition
          | Var String           -- variable
          | Num Int              -- constant
          deriving (Show)

expr :: Parser Expr
expr = atom `chainl1` (return App)

atom :: Parser Expr
atom = lam +++ local +++ var +++ paren +++ num

lam :: Parser Expr
lam = do
    symbol "\\"
    v <- variable
    symbol "->"
    exp <- atom
    return $ Lam v exp

local :: Parser Expr
local = do
    symbol "let"
    v <- variable
    symbol "="
    exp <- atom
    symbol "in"
    rexp <- atom
    return $ Let v exp rexp

var :: Parser Expr
var = Var <$> variable

paren :: Parser Expr
paren = symbol "(" *> atom <* symbol ")"

variable :: Parser String
variable = identifier ["let", "in"]

num :: Parser Expr
num = Num <$> natural

---------------------------------

{-
1 - 2 - 3 - 4
1 - ( 2 - ( 3 - 4 ) ) <- Left associative
( ( 1 - 2 ) - 3 ) - 4 <- Right associative

type LParser a = String -> Maybe (a, String)

lresult :: a -> LParser a
lresult v inp = Just (v,inp)

lzero :: LParser a
lzero = \inp -> Nothing

litem :: LParser Char
litem = \inp -> case inp of
                 []     -> Nothing
                 (x:xs) -> Just (x,xs)

-}
