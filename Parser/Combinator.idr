module Parser.Combinator

import Control.Algebra.NumericInstances
import Data.Vect

data Parser : (a : Type) -> Type where
  MkParser : (String -> List (a, String)) -> Parser a

Functor Parser where
  map f (MkParser g) = MkParser $ \input =>
    map (\(v, t) => (f v, t)) $ g input

item : Parser Char
item = MkParser $ \input =>
  case unpack input of
    []      => []
    x :: xs => [(x, pack xs)]

bind : Parser a -> (a -> Parser b) -> Parser b
bind p f = MkParser $ \inpt => ?rhs

pure : a -> Parser a
pure value
  = MkParser $ \input =>
               [(value, input)]

neutral : Parser a
neutral = MkParser $ \input => []

result : a -> Parser a
result v = MkParser $ \inp => [(v, inp)]

Parser : Type -> Type
Parser a = String -> List (a, String)

result : a -> Parser a
result v = \inp => [(v, inp)]

zero : Parser a
zero = \inp => []

item : Parser Char
item = \inp =>
  case unpack inp of
    [] => []
    x :: xs => [(x, pack xs)]

bind : Parser a -> (a -> Parser b) -> Parser b
bind p f = \inp => concat [f v inp' | (v, inp') <- p inp]

-- seq : Parser a -> Parser b -> ?hos
-- seq p q = bind p $ \x =>
--           bind q $ \y =>
--           result (x, y)

sat : (Char -> Bool) -> Parser Char
sat p = bind item $ \x =>
        if p x then result x else zero

char : Char -> Parser Char
char x = sat $ \y => x == y

digit : Parser Char
digit = sat $ \x => '0' <= x && x <= '9'

lower : Parser Char
lower = sat $ \x => 'a' <= x && x <= 'z'

upper : Parser Char
upper = sat $ \x => 'A' <= x && x <= 'Z'

plus : Parser a -> Parser a -> Parser a
plus p q = \inp => (p inp ++ q inp)

letter : Parser Char
letter = plus lower upper

alphanum : Parser Char
alphanum = plus letter digit

word : Parser String
word = plus neWord $ result ""
       where
         neWord : Parser String
         neWord = bind letter $ \x  =>
                  bind word   $ \xs =>
                  result $ pack $ x :: (unpack xs)

interface MMonad (m : Type -> Type) where
  mresult : a -> m a
  mbind   : m a -> (a -> m b) -> m b

-- - + Errors (1)
--  `-- ./Parser/Combinator.idr line 71 col 9:
--      Parser  cannot be a parameter of Parser.Combinator.MMonad
--      (Type class arguments must be injective)
-- instance MMonad Parser where
--   mresult v = \inp => [(v,inp)]
--   mbind p f = \inp => concat [f v out | (v, out) <- p inp]

-- string : String -> Parser String
-- string "" = \inp => [("", inp)]
-- string str with (unpack str)
--   | [] = \inp => [("", inp)]
--   | (x :: xs) = \inp => [("", inp)]

v : List Int
v = [3,3]
