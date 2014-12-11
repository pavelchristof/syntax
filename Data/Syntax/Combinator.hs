{- |
Module      :  Data.Syntax.Combinator
Description :  Combinators that work with any sequence type.
Copyright   :  (c) Daan Leijen 1999-2001, Bryan O'Sullivan 2007-2014, Paweł Nowak 2014
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental

Combinators that work with any sequence type.
-}
module Data.Syntax.Combinator where

import Control.Category.Structures
import Control.Lens
import Control.Lens.SemiIso
import Control.SIArrow

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

-- | Tries to apply the actions in the list in order, until one of
-- them succeeds. Returns the value of the succeeding action.
choice :: SIArrow cat => [cat () a] -> cat () a
choice = foldr (/+/) (sifail "choice: all alternatives failed")

-- | Combine two alternatives.
eitherOf :: SIArrow cat => cat () a -> cat () b -> cat () (Either a b)
eitherOf a b = _Left /$/ a /+/ _Right /$/ b
