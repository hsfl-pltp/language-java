module Language.Java.Syntax.Equality where

import Language.Java.SourceSpan (Localized (..))

data EqualityConstraint
  = IgnoreSourceSpan
  | IncludeSourceSpan

class Localized a => Equality a where
  eqImpl :: a -> a -> Bool

  eqListImpl :: [a] -> [a] -> Bool
  eqListImpl [] [] = True
  eqListImpl [] _ = False
  eqListImpl _ [] = False
  eqListImpl (a1 : as1) (a2 : as2) =
    eqImpl a1 a2 && eqListImpl as1 as2

  eq :: EqualityConstraint -> a -> a -> Bool
  eq IgnoreSourceSpan a1 a2 = eqImpl a1 a2
  eq IncludeSourceSpan a1 a2 = eqImpl a1 a2 && sourcespan a1 == sourcespan a2

  eqList :: EqualityConstraint -> [a] -> [a] -> Bool
  eqList _ [] [] = True
  eqList _ [] _ = False
  eqList _ _ [] = False
  eqList c (a1 : as1) (a2 : as2) =
    eq c a1 a2 && eqList c as1 as2
