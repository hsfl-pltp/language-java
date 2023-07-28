module Language.Java.Syntax.IdentCollection
  ( IdentCollection (..),
    addLocalVars,
    addToClassTrees,
    addToFields,
    addToFormalParams,
    addToTypeNames,
    isExpressionIdent,
    isTypeIdent,
    empty,
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
  { icFields :: [Ident],
    icFormalParams :: [Ident],
    icLocalVars :: [Ident],
    icTypeNames :: [Ident],
    icClassInfos :: [ClassInfo]
  }

addToFields :: [Ident] -> IdentCollection -> IdentCollection
addToFields idents identCollection = identCollection {icFields = idents ++ icFields identCollection}

addToFormalParams :: [Ident] -> IdentCollection -> IdentCollection
addToFormalParams idents identCollection = identCollection {icFormalParams = idents ++ icFormalParams identCollection}

addLocalVars :: [Ident] -> IdentCollection -> IdentCollection
addLocalVars idents identCollection = identCollection {icLocalVars = idents ++ icLocalVars identCollection}

addToTypeNames :: [Ident] -> IdentCollection -> IdentCollection
addToTypeNames idents identCollection = identCollection {icTypeNames = idents ++ icTypeNames identCollection}

addToClassTrees :: [ClassInfo] -> IdentCollection -> IdentCollection
addToClassTrees classTreesNew identCollection = identCollection {icClassInfos = classTreesNew ++ icClassInfos identCollection}

isExpressionIdent :: Ident -> IdentCollection -> Bool
isExpressionIdent idnt (IdentCollection cfields fp vars _ _) = any (eq IgnoreSourceSpan idnt) (cfields ++ fp ++ vars)

isTypeIdent :: Ident -> IdentCollection -> Bool
isTypeIdent idnt (IdentCollection _ _ _ typeVars _) = any (eq IgnoreSourceSpan idnt) typeVars

empty :: IdentCollection
empty = IdentCollection [] [] [] [] []