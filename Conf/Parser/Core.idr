module Conf.Parser.Core

-- Represent medium result of Parsing as dual data
-- structure not as list.
data Result a = Success a | Failure

instance Functor Result where
  map f (Success a) = Success $ f a
  map f Failure     = Failure

data Parser : (a : Type) -> Type where
  MkParser : (String -> List (a, String)) -> Parser a

data Parser' : (a : Type) -> Type where
  MkParser' : (String -> Result a) -> Parser' a

instance Functor Parser where
  map f (MkParser g) = MkParser $ \input =>
    map (\(v, t) => (f v, t)) $ g input

instance Functor Parser' where
  map f (MkParser' g) = MkParser' $ \input =>
    f <$> (g input)

-- instance Applicative Parser where
--   pure f = MkParser $ \input =>
--     [(f, input)]
--   (MkParser f) <*> (MkParser g) = MkParser $ \input =>
--     map (\(v, k) => ) $ g input

pure : a -> Parser a
pure value
  = MkParser $ \input =>
               [(value, input)]

neutral : Parser a
neutral = MkParser $ \input => []

item : Parser Char
item = MkParser $ \input =>
  case unpack input of
    []      => []
    x :: xs => [(x, pack xs)]

bind : Parser a -> (a -> Parser b) -> Parser b
bind p f = MkParser $ \inpt => ?rhs

process : String -> Parser Int
process str = ?rhs
