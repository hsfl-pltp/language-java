{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Java.Syntax.Types where

import Data.Data
import GHC.Generics (Generic)
import Language.Java.SourceSpan (Localized (..), SourceSpan)
import Language.Java.Syntax.Equality (Equality (..))

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type
  = PrimType PrimType
  | RefType RefType
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | There are three kinds of reference types: class types, interface types, and array types.
--   Reference types may be parameterized with type arguments.
--   Type variables cannot be syntactically distinguished from class type identifiers,
--   and are thus represented uniformly as single ident class types.
data RefType
  = ClassRefType ClassType
  | -- | TypeVariable Ident
    ArrayType Type
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | A class or interface type consists of a type declaration specifier,
--   optionally followed by type arguments (in which case it is a parameterized type).
data ClassType
  = ClassType [(Ident, [TypeArgument])]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | Type arguments may be either reference types or wildcards.
data TypeArgument
  = Wildcard (Maybe WildcardBound)
  | ActualType RefType
  deriving (Eq, Show, Read, Typeable, Generic, Data)

data TypeDeclSpecifier
  = TypeDeclSpecifier ClassType
  | TypeDeclSpecifierWithDiamond ClassType Ident Diamond
  | TypeDeclSpecifierUnqualifiedWithDiamond Ident Diamond
  deriving (Eq, Show, Read, Typeable, Generic, Data)

data Diamond = Diamond
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
data WildcardBound
  = ExtendsBound RefType
  | SuperBound RefType
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | A primitive type is predefined by the Java programming language and named by its reserved keyword.
data PrimType
  = BooleanT
  | ByteT
  | ShortT
  | IntT
  | LongT
  | CharT
  | FloatT
  | DoubleT
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | A class is generic if it declares one or more type variables. These type variables are known
--   as the type parameters of the class.
data TypeParam = TypeParam Ident [RefType]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
data Ident = Ident SourceSpan String
  deriving (Ord, Show, Read, Typeable, Generic, Data)

instance Localized Ident where
  sourcespan (Ident s _) = s

instance Equality Ident where
  eqImpl (Ident _ str1) (Ident _ str2) = str1 == str2

-- | A name, i.e. a period-separated list of identifiers.
data Name = Name SourceSpan [Ident]
  deriving (Ord, Show, Read, Typeable, Generic, Data)

instance Localized Name where
  sourcespan (Name s _) = s

instance Equality Name where
  eqImpl (Name _ ids1) (Name _ ids2) = eqListImpl ids1 ids2
