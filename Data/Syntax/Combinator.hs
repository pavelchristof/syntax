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

import Control.Lens.Cons
import Control.Lens.Empty
import Data.SemiIsoFunctor

-- | Zero or more occurences of v separated by s.
sepBy :: SemiIsoAlternative f => f a -> f () -> f [a]
sepBy v s =  sepBy1 v s 
         /|/ sipure _Empty

-- | One or more occurences of v separated by s.
sepBy1 :: SemiIsoAlternative f => f a -> f () -> f [a]
sepBy1 v s = _Cons /$/ v /*/ (s */ sepBy1 v s /|/ sipure _Empty)
