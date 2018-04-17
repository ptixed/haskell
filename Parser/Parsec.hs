{-# LANGUAGE FlexibleInstances #-}

--
-- ██████╗  █████╗ ██████╗ ███████╗███████╗ ██████╗
-- ██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝
-- ██████╔╝███████║██████╔╝███████╗█████╗  ██║
-- ██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██║
-- ██║     ██║  ██║██║  ██║███████║███████╗╚██████╗
-- ╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝ ╚═════╝

module Parser.Parsec
    ( module Parser.Parsec
    , module Control.Applicative
    ) where

import Control.Applicative hiding (many)
import Data.Char

-------------------------

data PError = PError Int String
 deriving (Show)
    
-------------------------

class RState state where
    readS :: state -> (Maybe Char, state)
    zeroS :: state
    
instance RState [Char] where
    readS []     = (Nothing, "")
    readS (x:xs) = (Just x, xs)
    zeroS        = ""

-------------------------

data PState state o =
    Valid Int state o
    | Error [PError]
    | MaybeError Int state o [PError]
    deriving (Show)

instance (RState state) => Functor (PState state) where
    fmap f (Valid n s o)         = Valid n s (f o)
    fmap f (MaybeError n s o es) = MaybeError n s (f o) es
    fmap f (Error es)            = Error es

instance (RState state) => Applicative (PState state) where
    pure = Valid 0 zeroS
    (Valid n s f)         <*> x                         = fmap f x
    (MaybeError n s f es) <*> (MaybeError n1 s1 o1 es1) = MaybeError n1 s1 (f o1) (es ++ es1)
    (MaybeError n s f es) <*> x                         = fmap f x
    (Error es)            <*> _                         = Error es

instance (RState state) => Alternative (PState state) where
    empty = Error []
    x@(Valid _ _ _)         <|> _                      = x
    x@(MaybeError n s o es) <|> (MaybeError _ _ _ es1) = MaybeError n s o (es ++ es1)
    x@(MaybeError _ _ _ _)  <|> _                      = x
    (Error _)               <|> x@(Valid _ _ _)        = x
    (Error xs)              <|> (Error ys)             = Error (xs ++ ys)

-------------------------

newtype ParserT state o1 o2 =
    ParserT (PState state o1 -> PState state o2)

instance (RState state) => Functor (ParserT state o0) where
    fmap f p = ParserT $ \s -> fmap f (invoke p s)

instance (RState state) => Applicative (ParserT state o0) where
    pure o  = ParserT $ \s -> (o <$ s)
    l <*> r = ParserT $ \s -> let s' = invoke l s in s' <*> (invoke r (s <* s'))

instance (RState state) => Alternative (ParserT state o0) where
    empty   = ParserT $ \_ -> empty
    l <|> r = ParserT $ \s -> invoke l s <|> invoke r s

-------------------------

sepBy1 :: (RState state) => ParserT state () a -> ParserT state () a1 -> ParserT state () [a]
sepBy1 p sep = (:) <$> p <*> (many (sep *> p))

sepBy :: (RState state) => ParserT state () a -> ParserT state () a1 -> ParserT state () [a]
sepBy p sep = sepBy1 p sep <|> pure []

many :: (RState state) => ParserT state () b -> ParserT state () [b]
many p = ParserT f where
    f s = case invoke p s of
        Error es                 -> case s of
            Valid n s _          -> MaybeError n s [] es
            MaybeError n s _ es1 -> MaybeError n s [] (es ++ es1)
            Error _              -> [] <$ s
        x@(MaybeError _ _ _ _)   -> [] <$ s
        x@(Valid _ _ _)          -> (:) <$> x <*> f (() <$ x)

many1 :: (RState state) => ParserT state () b -> ParserT state () [b]
many1 p = ParserT f where
    f s = (:) <$> x <*> invoke (many p) (() <$ x) where
        x = invoke p (() <$ s)

string :: (RState state) => [Char] -> ParserT state a [Char]
string str = ParserT f where
    f s          = foldl (\s p -> compose <$> s <*> invoke p s) ([] <$ s) (map char str)
    compose xs x = xs ++ [x]
    
char :: (RState state) => Char -> ParserT state a Char
char c = consume (==c) ("char " ++ [c])

option :: (RState state) => ParserT state () a -> ParserT state () (Maybe a)
option p = ParserT f where
    f s = case invoke p s of
        Error _            -> Nothing <$ s
        MaybeError _ _ _ _ -> Nothing <$ s
        x@(Valid _ _ _)    -> Just <$> x 

eof :: (RState state) => ParserT state a ()
eof = ParserT f where
    f (Error es)                = Error es
    f (MaybeError n state _ es) = g $ readS state where
        g (Just x,    _) = Error ([PError n "eof"] ++ es)
        g (Nothing, s)   = Valid n s ()
    f (Valid n state _)         = g $ readS state where
        g (Just x,    _) = Error [PError n "eof"]
        g (Nothing, s)   = Valid n s ()

consume :: (RState state) => (Char -> Bool) -> String -> ParserT state a Char
consume cond err = ParserT f where
    f (Error xs)                = Error xs
    f (MaybeError n state _ es) = g $ readS state where
        g (Nothing, _)   = Error ([PError n err] ++ es)
        g (Just x,    s) = if cond x 
            then Valid (n+1) s x
            else Error ([PError n err] ++ es)
    f (Valid n state _) = g $ readS state where 
        g (Nothing, _) = Error [PError n err]
        g (Just x,  s) = if cond x 
            then Valid (n+1) s x
            else Error [PError n err]

invoke :: (RState state) => ParserT state o1 o2 -> PState state o1 -> PState state o2
invoke (ParserT f) x = f x

pfail :: (RState state) => ParserT state a ()
pfail = ParserT f where 
    f x@(Error _)            = () <$ x
    f x@(MaybeError _ _ _ _) = () <$ x
    f (Valid n _ _)          = Error [PError n "fail"]

-------------------------

--
-- ██╗   ██╗████████╗██╗██╗     ███████╗
-- ██║   ██║╚══██╔══╝██║██║     ██╔════╝
-- ██║   ██║   ██║   ██║██║     ███████╗
-- ██║   ██║   ██║   ██║██║     ╚════██║
-- ╚██████╔╝   ██║   ██║███████╗███████║
--  ╚═════╝    ╚═╝   ╚═╝╚══════╝╚══════╝

oneOf :: (RState state) => [Char] -> ParserT state a Char
oneOf xs = consume (`elem` xs) ("oneOf " ++ xs)

noneOf :: (RState state) => [Char] -> ParserT state a Char
noneOf xs = consume (not . (`elem` xs)) ("noneOf " ++ xs)

space :: (RState state) => ParserT state a Char
space = consume isSpace "space"

spaces :: (RState state) => ParserT state () [Char]
spaces = many space

spaces1 :: (RState state) => ParserT state () [Char]
spaces1 = many1 space

alphaNum :: (RState state) => ParserT state () Char
alphaNum = consume (\x -> isNumber x || isLetter x) "alphaNum"

parse :: (RState state) => ParserT state () b -> state -> Either [PError] b
parse p state = case invoke p $ Valid 0 state () of
    Valid _ _ x        -> Right x
    MaybeError _ _ x _ -> Right x
    Error xs           -> Left xs
