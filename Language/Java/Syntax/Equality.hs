module Language.Java.Syntax.Equality where

data EqOptions
  = IgnoreSourceSpan
  | IncludeSourceSpan

class Equality a where
  eq :: EqOptions -> a -> a -> Bool

eqList :: Equality a => EqOptions -> [a] -> [a] -> Bool
eqList _ [] [] = True
eqList _ [] _ = False
eqList _ _ [] = False
eqList opt (a1 : as1) (a2 : as2) =
  eq opt a1 a2 && eqList opt as1 as2
