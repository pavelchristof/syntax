{- |
Module      :  Data.Syntax.Combinator
Description :  Combinators that work with any sequence type.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental

Combinators that work with any sequence type.
-}
module Data.Syntax.Combinator where

import Control.Lens
import Control.Lens.SemiIso
import Data.SemiIsoFunctor

-- | Zero or more occurences of @v@ separated by @s@.
sepBy :: SemiIsoAlternative f => f a -> f () -> f [a]
sepBy v s =  sepBy1 v s 
         /|/ sipure _Empty

-- | One or more occurences of @v@ separated by @s@.
sepBy1 :: SemiIsoAlternative f => f a -> f () -> f [a]
sepBy1 v s = _Cons /$/ v /*/ (s */ sepBy1 v s /|/ sipure _Empty)

-- | One or none occurences of @f@.
optional :: SemiIsoAlternative f => f a -> f (Maybe a)
optional f = _Just /$/ f /|/ sipure _Nothing

-- | Like 'optional', but specialized for @()@.
opt :: SemiIsoAlternative f => f () -> f ()
opt f = f /|/ sipure id

-- | Parser one or more occurences of @f@, but prints nothing.
opt_ :: SemiIsoAlternative f => f () -> f ()
opt_ f =  semiIso (const (Left "opt_")) Right /$/ f
      /|/ sipure id
