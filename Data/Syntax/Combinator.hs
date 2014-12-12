{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Module      :  Data.Syntax.Combinator
Description :  Combinators that work with any sequence type.
Copyright   :  (c) Daan Leijen 1999-2001, Bryan O'Sullivan 2007-2014, Paweł Nowak 2014
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental

Combinators that work with any sequence type.
-}
module Data.Syntax.Combinator
    (
    -- * Combinators.
    optional,
    opt,
    opt_,
    choice,
    eitherOf,

    -- * Lists.
    manyTill,
    sepBy,
    sepBy1,

    -- * Vectors.
    vecNSepBy,
    ivecNSepBy,
    vec,
    vecSepBy,
    ivec,
    ivecSepBy
    ) where

import           Control.Category
import           Control.Category.Structures
import           Control.Lens
import           Control.Lens.SemiIso
import           Control.SIArrow
import           Data.Syntax
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Prelude hiding (id, (.))

-- | One or zero occurences of @f@.
optional :: SIArrow cat => cat () a -> cat () (Maybe a)
optional f = _Just /$/ f /+/ sipure _Nothing

-- | Like 'optional', but specialized for @()@.
opt :: SIArrow cat => cat () () -> cat () ()
opt f = f /+/ sipure id

-- | Parser one or zero occurences of @f@, but prints nothing.
opt_ :: SIArrow cat => cat () () -> cat () ()
opt_ f =  semiIso (const (Left "opt_")) Right /$/ f
      /+/ sipure id

-- | Tries to apply the actions in the list in order, until one of
-- them succeeds. Returns the value of the succeeding action.
choice :: SIArrow cat => [cat () a] -> cat () a
choice = foldr (/+/) (sifail "choice: all alternatives failed")

-- | Combine two alternatives.
eitherOf :: SIArrow cat => cat () a -> cat () b -> cat () (Either a b)
eitherOf a b = _Left /$/ a /+/ _Right /$/ b

-- | @manyTill p end@ applies action p zero or more times until action
-- end succeeds, and returns the list of values returned by p.
manyTill :: SIArrow cat => cat () a -> cat () () -> cat () [a]
manyTill p end =  _Empty /$/ end
              /+/ _Cons /$/ p /*/ manyTill p end

-- | Zero or more occurences of @v@ separated by @s@.
sepBy :: SIArrow cat => cat () a -> cat () () -> cat () [a]
sepBy v s = sepBy1 v s /+/ sipure _Empty

-- | One or more occurences of @v@ separated by @s@.
sepBy1 :: SIArrow cat => cat () a -> cat () () -> cat () [a]
sepBy1 v s = _Cons /$/ v /*/ (s */ sepBy1 v s /+/ sipure _Empty)

-- | Constant size vector with separators.
--
-- @vecNSepBy n e sep@ describes a size @n@ vector with elements @e@ separated by @sep@.
vecNSepBy :: Syntax syn => Int -> syn () a -> syn () () -> syn () (Vector a)
vecNSepBy n e sep = ivecNSepBy n (unit ^>> second e) sep

-- | Constant size vector with separators and index-aware elements.
--
-- @ivecNSepBy n e sep@ describes a size @n@ vector with elements @e@ separated by @sep@.
-- Each element gets its index and should output a value and the index unchanged.
ivecNSepBy :: Syntax syn => Int -> syn Int (Int, a) -> syn () () -> syn () (Vector a)
ivecNSepBy n e sep = ivecN n $ sibind $ iso el (el . fst)
  where
    el k | k < n - 1 = unit ^>> e *** sep >># unit
         | otherwise = e

-- | Runtime sized vector. The size can depend on the result of some computation.
--
-- @vec e@ describes a vector with elements @e@.
vec :: Syntax syn => syn () a -> syn Int (Vector a)
vec e = vecSepBy e (siarr id)

-- | Runtime sized vector with separators. The size can depend on the result of some
-- computation.
--
-- @vecSepBy e sep@ describes a vector with elements @e@ separated by @sep@.
vecSepBy :: Syntax syn => syn () a -> syn () () -> syn Int (Vector a)
vecSepBy e sep = ivecSepBy (unit ^>> second e) sep

-- | Runtime sized vector with index-aware elements. The size can depend on the result
-- of some computation.
--
-- @ivec e@ describes a vector with elements @e@.
ivec :: Syntax syn => syn Int (Int, a) -> syn Int (Vector a)
ivec e = ivecSepBy e (siarr id)

-- | Runtime sized vector with index-aware elements and separators. The size can depend
-- on the result of some computation.
--
-- @ivecSepBy e sep@ describes a vector with elements @e@ separated by @sep@.
ivecSepBy :: Syntax syn => syn Int (Int, a) -> syn () () -> syn Int (Vector a)
ivecSepBy e sep = sibind $ iso (\n -> constant n #>> ivecNSepBy n e sep)
                               (\v -> constant (V.length v) #>> ivecNSepBy (V.length v) e sep)
