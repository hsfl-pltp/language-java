{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Java.Syntax.Exp where

import Data.Data
import GHC.Generics (Generic)
import Language.Java.SourceSpan (Located (..), SourceSpan)
import Language.Java.Syntax.Equality (Equality (..))

-- | A literal denotes a fixed, unchanging value.
data Literal
  = Int SourceSpan Integer
  | Word SourceSpan Integer
  | Float SourceSpan Double
  | Double SourceSpan Double
  | Boolean SourceSpan Bool
  | Char SourceSpan Char
  | String SourceSpan String
  | Null SourceSpan
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Literal where
  eq opt (Int s1 n1) (Int s2 n2) =
    eq opt s1 s2 && n1 == n2
  eq opt (Word s1 n1) (Word s2 n2) =
    eq opt s1 s2 && n1 == n2
  eq opt (Float s1 x1) (Float s2 x2) =
    eq opt s1 s2 && x1 == x2
  eq opt (Double s1 x1) (Double s2 x2) =
    eq opt s1 s2 && x1 == x2
  eq opt (Boolean s1 b1) (Boolean s2 b2) =
    eq opt s1 s2 && b1 == b2
  eq opt (Char s1 c1) (Char s2 c2) =
    eq opt s1 s2 && c1 == c2
  eq opt (String s1 str1) (String s2 str2) =
    eq opt s1 s2 && str1 == str2
  eq opt (Null s1) (Null s2) =
    eq opt s1 s2
  eq _ _ _ = False

instance Located Literal where
  sourceSpan (Int s _) = s
  sourceSpan (Word s _) = s
  sourceSpan (Float s _) = s
  sourceSpan (Double s _) = s
  sourceSpan (Boolean s _) = s
  sourceSpan (Char s _) = s
  sourceSpan (String s _) = s
  sourceSpan (Null s) = s

-- | A binary infix operator.
data Op
  = Mult
  | Div
  | Rem
  | Add
  | Sub
  | LShift
  | RShift
  | RRShift
  | LThan
  | GThan
  | LThanE
  | GThanE
  | Equal
  | NotEq
  | And
  | Or
  | Xor
  | CAnd
  | COr
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Op where
  eq _ Mult Mult = True
  eq _ Div Div = True
  eq _ Rem Rem = True
  eq _ Add Add = True
  eq _ Sub Sub = True
  eq _ LShift LShift = True
  eq _ RShift RShift = True
  eq _ RRShift RRShift = True
  eq _ LThan LThan = True
  eq _ GThan GThan = True
  eq _ LThanE LThanE = True
  eq _ GThanE GThanE = True
  eq _ Equal Equal = True
  eq _ NotEq NotEq = True
  eq _ And And = True
  eq _ Or Or = True
  eq _ Xor Xor = True
  eq _ CAnd CAnd = True
  eq _ COr COr = True
  eq _ _ _ = False

-- | An assignment operator.
data AssignOp
  = EqualA
  | MultA
  | DivA
  | RemA
  | AddA
  | SubA
  | LShiftA
  | RShiftA
  | RRShiftA
  | AndA
  | XorA
  | OrA
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality AssignOp where
  eq _ EqualA EqualA = True
  eq _ MultA MultA = True
  eq _ DivA DivA = True
  eq _ RemA RemA = True
  eq _ AddA AddA = True
  eq _ SubA SubA = True
  eq _ LShiftA LShiftA = True
  eq _ RShiftA RShiftA = True
  eq _ RRShiftA RRShiftA = True
  eq _ AndA AndA = True
  eq _ XorA XorA = True
  eq _ OrA OrA = True
  eq _ _ _ = False
