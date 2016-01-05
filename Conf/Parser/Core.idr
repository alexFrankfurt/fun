module Conf.Parser.Core

-- Represent medium result of Parsing as dual data
-- structure not as list.
-- Slightly unhappy that implementation of Functor
-- adds constraint: context goes first because I cannot
-- give an implementation something like:
-- ```idris
-- instance Functor Result _ a where
-- ...
-- ```
data Result : s -> a -> Type where
  Success : String -> a -> Result s a
  Failure : String -> Result s a


instance Functor (Result s) where
  map f (Success s a) = Success s $ f a
  map f (Failure s)   = Failure s       -- ones confused with id function

instance Applicative (Result s) where
  pure f = Success "" f
  -- needs improvements?
  (Success s f) <*> g = f <$> g
  (Failure s) <*> g = Failure s

data Parser : (a : Type) -> Type where
  MkParser : (String -> List (a, String)) -> Parser a

data Parser' : (a : Type) -> Type where
  MkParser' : (String -> Result String a) -> Parser' a

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

instance Applicative Parser' where
  pure v = MkParser' $ \input => Success input v
  (MkParser' f) <*> (MkParser' p) = MkParser' $ \input =>
    (f input) <*> (p input)
    --(\g => g <*> (p input)) <$> (f input)

join : Result s (Parser' a) -> Parser' a
join (Success s r) = r
join (Failure f) = MkParser' $ \input => Failure input

parserApply : Parser' a -> String -> Result String a
parserApply (MkParser' p) s = p s

instance Monad Parser' where
  -- (>>=) : m a -> (a -> m b) -> m b
  (MkParser' p) >>= f = MkParser' $ \input =>
    parserApply (join $ f <$> (p input)) input

pure : a -> Parser a
pure value
  = MkParser $ \input =>
               [(value, input)]

-- pure' : a -> Parser' a
-- pure' value
--   = MkParser' $ \input =>
--                 Success input value

neutral : Parser a
neutral = MkParser $ \input => []

neutral' : Parser' a
neutral' = MkParser' $ \input => Failure input

item : Parser Char
item = MkParser $ \input =>
  case unpack input of
    []      => []
    x :: xs => [(x, pack xs)]

item' : Parser' Char
item' = MkParser' $ \input =>
  case unpack input of
    []      => Failure ""
    x :: xs => Success (pack xs) x

-- sat : (Char -> Bool) -> Parser' Char
-- sat p = (>>=) item $ \x =>
--   if p x then pure x else neutral
--         -- do x <- item
--         --    if p x
--         --      then pure x
--         --      else neutral

bind : Parser a -> (a -> Parser b) -> Parser b
bind p f = MkParser $ \inpt => ?rhs

process : String -> Parser Int
process str = ?rhs
