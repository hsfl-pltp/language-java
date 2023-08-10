module Language.Java.Syntax.VarDecl (ident, isInitialized) where

import Language.Java.Syntax (Ident, VarDecl (..))
import qualified Language.Java.Syntax.VarDeclId as VarDeclId (ident)

ident :: VarDecl p -> Ident
ident (VarDecl _ varDeclIdent _) = VarDeclId.ident varDeclIdent

isInitialized :: VarDecl p -> Bool
isInitialized (VarDecl _ _ (Just _)) = True
isInitialized (VarDecl _ _ Nothing) = False