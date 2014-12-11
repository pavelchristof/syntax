{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
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

import Control.Lens.Iso
import Control.Lens.SemiIso
import Control.SIArrow
import Data.MonoTraversable
import Data.Sequences hiding (take, takeWhile)

-- | An isomorphism between a sequence and a list of its elements.
packed :: IsSequence seq => Iso' seq [Element seq]
packed = iso otoList fromList

-- | An abstract syntax description based on semi-isomorphisms.
--
-- This class can be implemented by both parsers and printers (and maybe more?).
-- 
-- The usual use is to write a polymorphic syntax description and instantiate it
-- both as a parser and a printer. An example is available in the 'syntax-example'
-- package.
--
-- Methods of this class try to mimic "Data.Attoparsec.Text" interface.
class ( SIArrow syn
      , IsSequence (Seq syn)
      , Eq (Seq syn)
      , Eq (Element (Seq syn)))
      => Syntax syn
    where
    -- | The sequence type used by this syntax.
    type Seq syn :: *

    -- | Any character.
    anyChar :: syn () (Element (Seq syn))

    -- | A specific character.
    char :: Element (Seq syn) -> syn () ()
    char c = rev (exact c) /$/ anyChar

    -- | Any character except the given one.
    notChar :: Element (Seq syn) -> syn () (Element (Seq syn))
    notChar c = bifiltered (/= c) /$/ anyChar

    -- | Any character satisfying a predicate.
    satisfy :: (Element (Seq syn) -> Bool) -> syn () (Element (Seq syn))
    satisfy p = bifiltered p /$/ anyChar

    -- | Transforms a character using a SemiIso and filters out values
    -- not satisfying the predicate.
    satisfyWith :: ASemiIso' a (Element (Seq syn)) -> (a -> Bool) -> syn () a
    satisfyWith ai p = bifiltered p . ai /$/ anyChar

    -- | A specific string.
    string :: (Seq syn) -> syn () ()
    string s = rev (exact s) /$/ take (olength s)

    -- | A string of length @n@.
    take :: Int -> syn () (Seq syn)
    take n = packed /$/ sireplicate n anyChar

    -- | Maximal string which elements satisfy a predicate.
    takeWhile :: (Element (Seq syn) -> Bool) -> syn () (Seq syn)
    takeWhile p = packed /$/ simany (satisfy p)

    -- | Maximal non-empty string which elements satisfy a predicate.
    takeWhile1 :: (Element (Seq syn) -> Bool) -> syn () (Seq syn)
    takeWhile1 p = packed /$/ sisome (satisfy p)

    -- | Maximal string which elements do not satisfy a predicate.
    takeTill :: (Element (Seq syn) -> Bool) -> syn () (Seq syn)
    takeTill p = takeWhile (not . p)

    -- | Maximal non-empty string which elements do not satisfy a predicate.
    takeTill1 :: (Element (Seq syn) -> Bool) -> syn () (Seq syn)
    takeTill1 p = takeWhile1 (not . p)

    {-# MINIMAL anyChar #-}
