module Language.Java.Syntax.IdentCollection
  ( IdentCollection (..),
    addToLocalVars,
    addToClassInfos,
    addToFields,
    addToFormalParams,
    isExpressionIdent,
    empty,
    addToImportedClasses,
    isImportedClass,
  )
where

import Language.Java.Syntax.ClassInfo (ClassInfo)
import Language.Java.Syntax.Equality
  ( EqOptions (IgnoreSourceSpan),
    Equality (eq),
  )
import Language.Java.Syntax.Types (Ident)

-- | data type used to collect the identifiers that are currently in scope
data IdentCollection = IdentCollection
  { icLocalVars :: [Ident],
    icFormalParams :: [Ident],
    icFields :: [Ident],
    icImportedClasses :: [Ident],
    icClassInfos :: [ClassInfo]
  }
  deriving (Show)

addToFields :: [Ident] -> IdentCollection -> IdentCollection
addToFields idents identCollection = identCollection {icFields = idents ++ icFields identCollection}

addToFormalParams :: [Ident] -> IdentCollection -> IdentCollection
addToFormalParams idents identCollection = identCollection {icFormalParams = idents ++ icFormalParams identCollection}

addToLocalVars :: [Ident] -> IdentCollection -> IdentCollection
addToLocalVars idents identCollection = identCollection {icLocalVars = idents ++ icLocalVars identCollection}

addToImportedClasses :: [Ident] -> IdentCollection -> IdentCollection
addToImportedClasses idents identCollection = identCollection {icImportedClasses = idents ++ icImportedClasses identCollection}

addToClassInfos :: [ClassInfo] -> IdentCollection -> IdentCollection
addToClassInfos classTreesNew identCollection = identCollection {icClassInfos = classTreesNew ++ icClassInfos identCollection}

isExpressionIdent :: Ident -> IdentCollection -> Bool
isExpressionIdent idnt ic =
  any (eq IgnoreSourceSpan idnt) (icLocalVars ic)
    || any (eq IgnoreSourceSpan idnt) (icFormalParams ic)
    || any (eq IgnoreSourceSpan idnt) (icFields ic)

isImportedClass :: Ident -> IdentCollection -> Bool
isImportedClass idnt ic = any (eq IgnoreSourceSpan idnt) (icImportedClasses ic)

empty :: IdentCollection
empty = IdentCollection [] [] [] [] []