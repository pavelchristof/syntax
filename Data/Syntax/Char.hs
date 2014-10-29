{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{- |
Module      :  Data.Syntax.Char
Description :  Char specific combinators.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental

Common combinators that work with sequences of chars.

There are A LOT of combinators missing.
-}
module Data.Syntax.Char (
    SyntaxChar,
    spaces,
    spaces_,
    spaces1,
    spaces1_,
    endOfLine
    ) where

import           Control.Lens.SemiIso
import           Data.Char
import           Data.MonoTraversable
import           Data.Monoid
import           Data.SemiIsoFunctor
import           Data.Syntax (Syntax)
import qualified Data.Syntax as S

-- | Syntax constrainted to sequences of chars.
type SyntaxChar syn seq = (Syntax syn seq, Element seq ~ Char)

-- | Accepts zero or more spaces. Generates a single space.
spaces :: SyntaxChar syn seq => syn ()
spaces = constant (opoint ' ') /$/ S.takeWhile isSpace

-- | Accepts zero or more spaces. Generates no output.
spaces_ :: SyntaxChar syn seq => syn ()
spaces_ = constant mempty /$/ S.takeWhile isSpace

-- | Accepts one or more spaces. Generates a single space.
spaces1 :: SyntaxChar syn seq => syn ()
spaces1 = constant (opoint ' ') /$/ S.takeWhile1 isSpace

-- | Accepts one or more spaces. Generates no output.
spaces1_ :: SyntaxChar syn seq => syn ()
spaces1_ = constant mempty /$/ S.takeWhile1 isSpace

-- | Accepts a single newline. Generates a newline.
endOfLine :: SyntaxChar syn seq => syn ()
endOfLine = S.char '\n'
