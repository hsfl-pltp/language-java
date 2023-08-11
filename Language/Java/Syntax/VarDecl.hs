module Language.Java.Syntax.VarDecl (ident, isInitialized, hasNoSideEffect) where

import Language.Java.Syntax (Ident, VarDecl (..))
import qualified Language.Java.Syntax.VarDeclId as VarDeclId (ident)
import qualified Language.Java.Syntax.VarInit as VarInit

ident :: VarDecl p -> Ident
ident (VarDecl _ varDeclIdent _) = VarDeclId.ident varDeclIdent

isInitialized :: VarDecl p -> Bool
isInitialized (VarDecl _ _ (Just _)) = True
isInitialized (VarDecl _ _ Nothing) = False

hasNoSideEffect :: VarDecl p -> Bool
hasNoSideEffect (VarDecl _ _ Nothing) = True
hasNoSideEffect (VarDecl _ _ (Just varInit)) = VarInit.hasNoSideEffect varInit
