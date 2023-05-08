{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Java.Syntax.Exp where

import Data.Data
import GHC.Generics (Generic)
import Language.Java.Syntax.Equality (Equality (..))

-- | A literal denotes a fixed, unchanging value.
data Literal
  = Int Integer
  | Word Integer
  | Float Double
  | Double Double
  | Boolean Bool
  | Char Char
  | String String
  | Null
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Literal where
  eq _ (Int n1) (Int n2) = n1 == n2
  eq _ (Word n1) (Word n2) = n1 == n2
  eq _ (Float x1) (Float x2) = x1 == x2
  eq _ (Double x1) (Double x2) = x1 == x2
  eq _ (Boolean b1) (Boolean b2) = b1 == b2
  eq _ (Char c1) (Char c2) = c1 == c2
  eq _ (String str1) (String str2) = str1 == str2
  eq _ Null Null = True
  eq _ _ _ = False

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
