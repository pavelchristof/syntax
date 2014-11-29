{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Syntax.Indent where

import Control.Lens.SemiIso
import Data.Char (isSpace)
import Data.SemiIsoFunctor
import Data.Sequences (fromList)
import Data.Syntax
import Data.Syntax.Char
import Data.Syntax.Combinator
import Prelude hiding (takeWhile, take)

newtype Indent m a = Indent { unIndent :: (Int, m ()) -> m a }

instance SemiIsoFunctor m => SemiIsoFunctor (Indent m) where
    simap f (Indent g) = Indent $ \i -> simap f (g i)

instance SemiIsoApply m => SemiIsoApply (Indent m) where
    sipure ai = Indent $ \_ -> sipure ai
    Indent f /*/ Indent g = Indent $ \i -> f i /*/ g i

instance SemiIsoAlternative m => SemiIsoAlternative (Indent m) where
    siempty = Indent $ \_ -> siempty
    Indent f /|/ Indent g = Indent $ \i -> f i /|/ g i

instance SemiIsoMonad m => SemiIsoMonad (Indent m) where
    (Indent m) //= f = Indent $ \i -> m i //= (\x -> unIndent (f x) i)

instance SemiIsoFix m => SemiIsoFix (Indent m) where
    sifix f = Indent $ \i -> sifix $ \y -> unIndent (f y) i

instance Syntax syn seq => Syntax (Indent syn) seq where
    anyChar = Indent $ const anyChar
    char = Indent . const . char
    notChar = Indent . const . notChar
    satisfy = Indent . const . satisfy
    satisfyWith ai = Indent . const . satisfyWith ai
    string = Indent . const . string
    take = Indent . const . take
    takeWhile = Indent . const . takeWhile
    takeWhile1 = Indent . const . takeWhile1
    takeTill = Indent . const . takeTill
    takeTill1 = Indent . const . takeTill1

instance SyntaxChar syn seq => SyntaxChar (Indent syn) seq where
    decimal = Indent $ const decimal
    scientific = Indent $ const scientific

breakLine :: SyntaxChar syn seq => Indent syn ()
breakLine = Indent $ \(i, tab) -> opt (char '\n') /* opt (sireplicate_ i tab) /* spaces_

indented :: Indent m a -> Indent m a
indented (Indent f) = Indent $ \(i, tab) -> f (i + 1, tab)

runIndent :: Indent m a -> m () -> m a
runIndent = ($ 0) . curry . unIndent
