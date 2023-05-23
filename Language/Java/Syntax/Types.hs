{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Java.Syntax.Types where

import Data.Data
import GHC.Generics (Generic)
import Language.Java.SourceSpan (Located (..), SourceSpan)
import Language.Java.Syntax.Equality (Equality (..))

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type
  = PrimType PrimType
  | RefType RefType
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Type where
  eq opt (PrimType pt1) (PrimType pt2) =
    eq opt pt1 pt2
  eq opt (RefType rt1) (RefType rt2) =
    eq opt rt1 rt2
  eq _ _ _ = False

-- | There are three kinds of reference types: class types, interface types, and array types.
--   Reference types may be parameterized with type arguments.
--   Type variables cannot be syntactically distinguished from class type identifiers,
--   and are thus represented uniformly as single ident class types.
data RefType
  = ClassRefType ClassType
  | -- | TypeVariable Ident
    ArrayType Type
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality RefType where
  eq opt (ClassRefType ct1) (ClassRefType ct2) =
    eq opt ct1 ct2
  eq opt (ArrayType t1) (ArrayType t2) =
    eq opt t1 t2
  eq _ _ _ = False

-- | A class or interface type consists of a type declaration specifier,
--   optionally followed by type arguments (in which case it is a parameterized type).
newtype ClassType
  = ClassType [(Ident, [TypeArgument])]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ClassType where
  eq opt (ClassType ctss1) (ClassType ctss2) =
    eq opt ctss1 ctss2

-- | Type arguments may be either reference types or wildcards.
data TypeArgument
  = Wildcard (Maybe WildcardBound)
  | ActualType RefType
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality TypeArgument where
  eq opt (Wildcard mwb1) (Wildcard mwb2) =
    eq opt mwb1 mwb2
  eq opt (ActualType rt1) (ActualType rt2) =
    eq opt rt1 rt2
  eq _ _ _ = False

data TypeDeclSpecifier
  = TypeDeclSpecifier ClassType
  | TypeDeclSpecifierWithDiamond ClassType Ident Diamond
  | TypeDeclSpecifierUnqualifiedWithDiamond Ident Diamond
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality TypeDeclSpecifier where
  eq opt (TypeDeclSpecifier ct1) (TypeDeclSpecifier ct2) =
    eq opt ct1 ct2
  eq opt (TypeDeclSpecifierWithDiamond ct1 i1 d1) (TypeDeclSpecifierWithDiamond ct2 i2 d2) =
    eq opt ct1 ct2 && eq opt i1 i2 && eq opt d1 d2
  eq opt (TypeDeclSpecifierUnqualifiedWithDiamond i1 d1) (TypeDeclSpecifierUnqualifiedWithDiamond i2 d2) =
    eq opt i1 i2 && eq opt d1 d2
  eq _ _ _ = False

data Diamond = Diamond
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Diamond where
  eq _ _ _ = True

-- | Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
data WildcardBound
  = ExtendsBound RefType
  | SuperBound RefType
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality WildcardBound where
  eq opt (ExtendsBound rt1) (ExtendsBound rt2) =
    eq opt rt1 rt2
  eq opt (SuperBound rt1) (SuperBound rt2) =
    eq opt rt1 rt2
  eq _ _ _ = False

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
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality PrimType where
  eq _ BooleanT BooleanT = True
  eq _ ByteT ByteT = True
  eq _ ShortT ShortT = True
  eq _ IntT IntT = True
  eq _ LongT LongT = True
  eq _ CharT CharT = True
  eq _ FloatT FloatT = True
  eq _ DoubleT DoubleT = True
  eq _ _ _ = False

-- | A class is generic if it declares one or more type variables. These type variables are known
--   as the type parameters of the class.
data TypeParam = TypeParam Ident [RefType]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality TypeParam where
  eq opt (TypeParam i1 rts1) (TypeParam i2 rts2) =
    eq opt i1 i2 && eq opt rts1 rts2

-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
data Ident = Ident SourceSpan String
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Ident where
  eq opt (Ident s1 str1) (Ident s2 str2) =
    eq opt s1 s2 && str1 == str2

instance Located Ident where
  sourceSpan (Ident s _) = s

-- | A name, i.e. a period-separated list of identifiers.
data Name = Name SourceSpan [Ident]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Name where
  eq opt (Name s1 ids1) (Name s2 ids2) =
    eq opt s1 s2 && eq opt ids1 ids2

instance Located Name where
  sourceSpan (Name s _) = s

data ClassifiedName
  = ExpressionName Name
  | TypeName Name
  | PackageName Name
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ClassifiedName where
  eq opt (ExpressionName n1) (ExpressionName n2) =
    eq opt n1 n2
  eq opt (TypeName n1) (TypeName n2) =
    eq opt n1 n2
  eq opt (PackageName n1) (PackageName n2) =
    eq opt n1 n2
  eq _ _ _ = False
