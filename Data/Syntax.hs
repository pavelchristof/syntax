{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{- |
Module      :  Data.Syntax
Description :  Abstract syntax description.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental

Abstract syntax descriptions based on semi-isomorphisms.
-}
module Data.Syntax (
    -- * Syntax.
    Syntax(..),
    -- * Common isomorphisms.
    packed
    ) where

import Prelude hiding (take, takeWhile)

import Control.Lens
import Control.Lens.SemiIso
import Data.MonoTraversable
import Data.SemiIsoFunctor
import Data.Sequences hiding (take, takeWhile)

-- | An isomorphism between a sequence and a list of its elements.
packed :: IsSequence seq => Iso' seq [Element seq]
packed = iso otoList fromList

-- | An abstract syntax description based on semi-isomorphisms.
--
-- This class can be implemented by both parsers and printers (and maybe more?).
-- 
-- The usual use is to write a polymorphic syntax description and instantiate it
-- both as a parser and a printer. An example syntax description:
--
-- > | A simple untyped lambda calculus.
-- > data AST = Var Text
-- >          | App AST AST
-- >          | Abs Text AST
-- >     deriving (Show)
-- >
-- > $(makePrisms ''AST)
-- >
-- > -- | A variable name.
-- > name :: Syntax syn Text => syn Text
-- > name = S.takeWhile1 isAlphaNum
-- >
-- > -- | Encloses a symbol in parentheses.
-- > parens :: Syntax syn Text => syn a -> syn a
-- > parens m = S.char '(' */ S.spaces_ */ m /* S.spaces_ /* S.char ')'
-- >
-- > -- | An atom is a variable or an expression in parentheses.
-- > atom :: Syntax syn Text => syn AST
-- > atom =  _Var /$/ name
-- >     /|/ parens expr
-- >
-- > -- | An expression of our lambda calculus.
-- > expr :: Syntax syn Text => syn AST
-- > expr =  _App /$/ atom /* S.spaces1 /*/ atom
-- >     /|/ _Abs /$/ S.char '\\'   /* S.spaces_
-- >               */ name          /* S.spaces
-- >              /*  S.string "->" /* S.spaces
-- >              /*/ expr 
-- >     /|/ atom
--
-- Methods of this class try to mimic "Data.Attoparsec.Text" interface.
class ( SemiIsoAlternative syn
      , IsSequence seq
      , Eq seq
      , Eq (Element seq)) 
      => Syntax syn seq | syn -> seq 
    where
    
    -- | Any character.
    anyChar :: syn (Element seq)

    -- | A specific character.
    char :: Element seq -> syn ()
    char c = exact c /$/ anyChar

    -- | Any character except the given one.
    notChar :: Element seq -> syn (Element seq)
    notChar c = filtered (/= c) /$/ anyChar

    -- | Any character satisfying a predicate.
    satisfy :: (Element seq -> Bool) -> syn (Element seq)
    satisfy p = filtered p /$/ anyChar

    -- | Transforms a character using a SemiIso and filters out values
    -- not satisfying the predicate.
    satisfyWith :: ASemiIso' a (Element seq) -> (a -> Bool) -> syn a
    satisfyWith ai p = filtered p . ai /$/ anyChar

    -- | A specific string.
    string :: seq -> syn ()
    string s = exact s /$/ take (olength s)

    -- | A string of length @n@.
    take :: Int -> syn seq
    take n = packed /$/ sireplicate n anyChar

    -- | Maximal string which elements satisfy a predicate.
    takeWhile :: (Element seq -> Bool) -> syn seq
    takeWhile p = packed /$/ simany (satisfy p)

    -- | Maximal non-empty string which elements satisfy a predicate.
    takeWhile1 :: (Element seq -> Bool) -> syn seq
    takeWhile1 p = packed /$/ sisome (satisfy p)

    -- | Maximal string which elements do not satisfy a predicate.
    takeTill :: (Element seq -> Bool) -> syn seq
    takeTill p = takeWhile (not . p)

    -- | Maximal non-empty string which elements do not satisfy a predicate.
    takeTill1 :: (Element seq -> Bool) -> syn seq
    takeTill1 p = takeWhile1 (not . p)

    {-# MINIMAL anyChar #-}
