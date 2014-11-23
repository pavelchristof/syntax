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

import Control.Lens
import Control.Lens.SemiIso
import Data.SemiIsoFunctor

-- | One or zero occurences of @f@.
optional :: SemiIsoAlternative f => f a -> f (Maybe a)
optional f = _Just /$/ f /|/ sipure _Nothing

-- | Like 'optional', but specialized for @()@.
opt :: SemiIsoAlternative f => f () -> f ()
opt f = f /|/ sipure id

-- | Parser one or zero occurences of @f@, but prints nothing.
opt_ :: SemiIsoAlternative f => f () -> f ()
opt_ f =  semiIso (const (Left "opt_")) Right /$/ f
      /|/ sipure id

-- | @manyTill p end@ applies action p zero or more times until action
-- end succeeds, and returns the list of values returned by p.
manyTill :: SemiIsoAlternative f => f a -> f () -> f [a]
manyTill p end =  _Empty /$/ end
              /|/ _Cons /$/ p /*/ manyTill p end

-- | Zero or more occurences of @v@ separated by @s@.
sepBy :: SemiIsoAlternative f => f a -> f () -> f [a]
sepBy v s = sepBy1 v s /|/ sipure _Empty

-- | One or more occurences of @v@ separated by @s@.
sepBy1 :: SemiIsoAlternative f => f a -> f () -> f [a]
sepBy1 v s = _Cons /$/ v /*/ (s */ sepBy1 v s /|/ sipure _Empty)

-- | Tries to apply the actions in the list in order, until one of
-- them succeeds. Returns the value of the succeeding action.
choice :: SemiIsoAlternative f => [f a] -> f a
choice = foldr (/|/) (sifail "choice: all alternatives failed")

-- | Combine two alternatives.
eitherOf :: SemiIsoAlternative f => f a -> f b -> f (Either a b)
eitherOf a b = _Left /$/ a /|/ _Right /$/ b
