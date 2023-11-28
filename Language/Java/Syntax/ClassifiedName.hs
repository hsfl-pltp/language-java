module Language.Java.Syntax.ClassifiedName where

import Language.Java.Syntax (ClassifiedExpressionName (LocalFieldAccess), ClassifiedName (..))

isField :: ClassifiedName -> Bool
isField (ExpressionName (LocalFieldAccess {})) = True
isField _ = False

isExpressionName :: ClassifiedName -> Bool
isExpressionName (ExpressionName _) = True
isExpressionName _ = False

isTypeName :: ClassifiedName -> Bool
isTypeName (TypeName _) = True
isTypeName _ = False

isUnknownName :: ClassifiedName -> Bool
isUnknownName (Unknown _) = True
isUnknownName _ = False
