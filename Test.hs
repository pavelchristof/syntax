{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Test where

import           Prelude hiding (id, (.), take, takeWhile)

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Control.Lens hiding (imap, _Cons)
import           Control.Monad
import qualified Data.Attoparsec.Text as Text
import           Data.Attoparsec.Types
import           Data.Char
import           Data.Functor.Identity
import           Data.MonoTraversable
import           Data.Sequences hiding (take, takeWhile)
import           Data.Text (Text)

infixl 3 /|/
infixl 4 /$/
infixl 5 /*/, /*, */

-- | Isomorphism in the kleisli category of some monad.
data KleisliIso m a b = KleisliIso (Kleisli m a b) (Kleisli m b a)

instance Monad m => Category (KleisliIso m) where
    id = KleisliIso id id
    (KleisliIso f g) . (KleisliIso f' g') = KleisliIso (f . f') (g' . g)

isoK :: (a -> m b) -> (b -> m a) -> KleisliIso m a b
isoK f g = KleisliIso (Kleisli f) (Kleisli g)

apply :: KleisliIso m a b -> a -> m b
apply (KleisliIso (Kleisli f) _) = f

unapply :: KleisliIso m a b -> b -> m a
unapply (KleisliIso _ (Kleisli g)) = g

from :: KleisliIso m a b -> KleisliIso m b a
from (KleisliIso f g) = KleisliIso g f

unit :: Monad m => KleisliIso m (a, ()) a
unit = isoK (return . fst) (return . (, ()))

symmetry :: Monad m => KleisliIso m (a, b) (b, a)
symmetry = isoK f f
  where f (a, b) = return (b, a)

equalTo :: (Eq a, Monad m) => a -> KleisliIso m a ()
equalTo x = isoK f g
  where 
    f y | x == y = return ()
        | otherwise = fail "equalTo: not equal"
    g _ = return x

oneWayConst :: Monad m => a -> KleisliIso m a ()
oneWayConst x = isoK f g
  where
    f _ = return ()
    g _ = return x

check :: Monad m => (a -> Bool) -> KleisliIso m a a
check p = isoK f f
  where f x | p x = return x
            | otherwise = fail "check: predicate failed"

packed :: (Monad m, IsSequence seq) => KleisliIso m [Element seq] seq
packed = isoK (return . fromList) (return . otoList)
              
-- | Isomorphism between unit and empty list.
_Nil :: Monad m => KleisliIso m () [a]
_Nil = isoK f g
  where
    f _  = return []
    g [] = return ()
    g _  = fail "expected empty list"

-- | Isomorphism between (head, tail) and a non empty list. 
_Cons :: Monad m => KleisliIso m (a, [a]) [a]
_Cons = isoK f g
  where
    f (x, xs) = return (x:xs)
    g (x:xs)  = return (x, xs)
    g _       = fail "expected non-empty list"

-- | A functor from the category of isomorphisms of the Kleisli category
-- of some monad to Hask.
class IsoFunctor f where
    imap  :: KleisliIso Identity a b -> f a -> f b
    imapK :: KleisliIso m a b -> f a -> f (m b)

(/$/) :: IsoFunctor f => KleisliIso Identity a b -> f a -> f b
(/$/) = imap

class IsoFunctor f => IsoPointed f where
    iunit :: f ()
    iunit = ipure id

    ipure  :: KleisliIso Identity () a -> f a
    ipure = (/$/ iunit)

    ipureK :: KleisliIso m () a -> f (m a)
    ipureK = (`imapK` iunit)

    {-# MINIMAL iunit | ipure #-}

class IsoFunctor f => IsoFail f where
    ifail :: String -> f a

class IsoPointed f => IsoApply f where
    (/*/) :: f a -> f b -> f (a, b)
    
    (/*)  :: f a -> f () -> f a
    f /* g = unit /$/ f /*/ g

    (*/)  :: f () -> f b -> f b
    f */ g = unit . symmetry /$/ f /*/ g
    
    {-# MINIMAL (/*/) #-}

class IsoApply f => IsoAlternative f where
    iempty :: f a
    (/|/) :: f a -> f a -> f a

    isome :: f a -> f [a]
    isome v = _Cons /$/ v /*/ imany v

    imany :: f a -> f [a]
    imany v = isome v /|/ ipure _Nil
    
    sepBy :: f a -> f () -> f [a]
    sepBy v s = sepBy1 v s /|/ ipure _Nil

    sepBy1 :: f a -> f () -> f [a]
    sepBy1 v s = _Cons /$/ v /* s /*/ sepBy v s

    {-# MINIMAL iempty, (/|/) #-}

class (IsoAlternative syn, IsoFail syn) => Syntax syn seq | syn -> seq where
    joinErr :: syn (Either String a) -> syn a

    anyChar :: syn (Element seq)

    char :: Element seq -> syn ()
    default char :: Eq (Element seq) => Element seq -> syn ()
    char c = equalTo c /$/ anyChar

    satisfy :: (Element seq -> Bool) -> syn (Element seq)
    satisfy p = joinErr (imapK (check p) anyChar)

    string :: seq -> syn ()
    default string :: (IsSequence seq, Eq seq) => seq -> syn ()
    string s = equalTo s /$/ take (olength s)

    take :: Int -> syn seq
    default take :: IsSequence seq => Int -> syn seq
    take n = packed /$/ ireplicate n anyChar

    takeWhile :: (Element seq -> Bool) -> syn seq
    default takeWhile :: IsSequence seq => (Element seq -> Bool) -> syn seq
    takeWhile p = packed /$/ imany (satisfy p)

    takeWhile1 :: (Element seq -> Bool) -> syn seq
    default takeWhile1 :: IsSequence seq => (Element seq -> Bool) -> syn seq
    takeWhile1 p = packed /$/ isome (satisfy p)
    
    {-# MINIMAL joinErr, anyChar #-}

ireplicate :: IsoApply f => Int -> f a -> f [a]
ireplicate 0 _ = ipure _Nil
ireplicate n m = _Cons /$/ m /*/ ireplicate (n-1) m

isequence :: IsoApply f => [f a] -> f [a]
isequence [] = ipure _Nil
isequence (x:xs) = _Cons /$/ x /*/ isequence xs

skipSpace :: (Syntax syn seq, IsSequence seq, Element seq ~ Char) => syn ()
skipSpace = oneWayConst (fromList []) /$/ takeWhile isSpace

skipSpace1 :: (Syntax syn seq, IsSequence seq, Element seq ~ Char) => syn ()
skipSpace1 = oneWayConst (singleton ' ') /$/ takeWhile1 isSpace

-- Attoparsec implementation.

instance IsoFunctor (Parser i) where
    imap i m  = fmap (runIdentity . apply i) m
    imapK i m = fmap (apply i) m

instance IsoFail (Parser i) where
    ifail = fail

instance IsoPointed (Parser i) where
    ipure = pure . runIdentity . (`apply` ())

instance IsoApply (Parser i) where
    f /*/ g = (,) <$> f <*> g

instance IsoAlternative (Parser i) where
    iempty = empty
    (/|/) = (<|>)
    imany = many
    isome = some
    sepBy = Text.sepBy
    sepBy1 = Text.sepBy1

instance Syntax (Parser Text) Text where
    joinErr = (>>= either fail return)
    anyChar = Text.anyChar
    satisfy = Text.satisfy
    char = void . Text.char
    string = void . Text.string
    takeWhile = Text.takeWhile
    takeWhile1 = Text.takeWhile1

-- Some AST.

data AST = Var Text
         | App AST AST
         | Abs Text AST
    deriving (Show)

_Var :: Monad m => KleisliIso m Text AST
_Var = isoK f g
  where
    f = return . Var
    g (Var t) = return t
    g _       = fail "expected a Var"

_App :: Monad m => KleisliIso m (AST, AST) AST
_App = isoK f g
  where
    f = return . uncurry App
    g (App h x) = return (h, x)
    g _         = fail "expected an App"

_Abs :: Monad m => KleisliIso m (Text, AST) AST
_Abs = isoK f g
  where
    f = return . uncurry Abs
    g (Abs n b) = return (n, b)
    g _         = fail "expected an Abs"

name :: Syntax syn Text => syn Text
name = takeWhile1 isAlphaNum

atom :: Syntax syn Text => syn AST
atom =  _Var /$/ name
    /|/ char '(' */ expr /* char ')'

expr :: Syntax syn Text => syn AST
expr =  _App /$/ atom /* skipSpace1 /*/ atom
    /|/ _Abs /$/ char '\\'   /* skipSpace 
              */ name        /* skipSpace 
             /*  string "->" /* skipSpace
             /*/ expr 
    /|/ atom
