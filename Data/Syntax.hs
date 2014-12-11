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

Reversible parsing and pretty-printing.
-}
module Data.Syntax (
    -- * Syntax.
    Syntax(..),
    Isolable(..),
    -- * Common isomorphisms.
    packed
    ) where

import Prelude hiding (take, takeWhile)

import Control.Category.Reader
import Control.Category.Structures
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
-- This class can be implemented by both parsers and printers.
-- 
-- The usual use is to write a polymorphic syntax description and instantiate it
-- both as a parser and a printer. Examples are available in 'syntax-example' and
-- 'syntax-example-json' packages.
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
    string :: Seq syn -> syn () ()
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

instance Syntax syn => Syntax (ReaderCT env syn) where
    type Seq (ReaderCT env syn) = Seq syn
    anyChar = clift anyChar
    char = clift . char
    notChar = clift . notChar
    satisfy = clift . satisfy
    satisfyWith ai = clift . satisfyWith ai
    string = clift . string
    take = clift . take
    takeWhile = clift . takeWhile
    takeWhile1 = clift . takeWhile1
    takeTill = clift . takeTill
    takeTill1 = clift . takeTill1

-- | Execute a computation in an isolated context.
--
-- The motivating example: you want to write a function
--
-- > serializeList :: Syntax syn => syn () a -> syn () [a]
--
-- Notice that we cannot just use simany, because the first @syn () a@
-- could eat the entire sequence (even though we printed more than 1 value!), so
-- we have to insert some kind of separators between the element. But how can we
-- be sure that @syn () a@ will not eat our separators? We can't! Thats why we
-- have to do the parsing in two stages: first extract the sequence between separators,
-- then run the @syn () a@ on this sequence.
class Syntax syn => Isolable syn where
    -- | Turns a computation into one executed in an isolated context.
    isolate :: syn () b -> syn (Seq syn) b
