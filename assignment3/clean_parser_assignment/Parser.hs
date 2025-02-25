module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-), comment) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

-- do the parser one or more times until it fails
iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

spaces :: Parser String
spaces = iter $ comment ! (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces 

letter :: Parser Char
letter =  char ? isAlpha

-- returns the first word with space removed after it
word :: Parser String
word = token (letter # iter letter >-> cons)

--applies char on a string n times and transforms it to string with cons
-- doesn't handel when n is larger than the length
{-
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons
-}

chars :: Int -> Parser String
chars n cs
    | n > (length cs) = Nothing
    | otherwise = return (take n cs) (drop n cs)

-- parses the upcoming chars if the they are the same as input-string
accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String
require w  = accept w ! (err $ "Program error: expecting " ++ w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

--apply char until '\n' comes up
comment :: Parser Char
comment = (accept "--") -# iter (char ? (/= '\n')) #- (char ? (=='\n')) >-> const ' '
