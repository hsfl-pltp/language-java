module Language.Java.Syntax.Equality where

import Data.List.NonEmpty (NonEmpty ((:|)))

data EqOptions
  = IgnoreSourceSpan
  | IncludeSourceSpan

class Equality a where
  eq :: EqOptions -> a -> a -> Bool

instance Equality a => Equality [a] where
  eq _ [] [] = True
  eq _ [] _ = False
  eq _ _ [] = False
  eq opt (a1 : as1) (a2 : as2) =
    eq opt a1 a2 && eq opt as1 as2

instance Equality a => Equality (NonEmpty a) where
  eq opt (a1 :| as1) (a2 :| as2) =
    eq opt a1 a2 && eq opt as1 as2

instance Equality a => Equality (Maybe a) where
  eq _ Nothing Nothing = True
  eq opt (Just a1) (Just a2) = eq opt a1 a2
  eq _ _ _ = False

instance (Equality a, Equality b) => Equality (a, b) where
  eq opt (a1, b1) (a2, b2) =
    eq opt a1 a2 && eq opt b1 b2
