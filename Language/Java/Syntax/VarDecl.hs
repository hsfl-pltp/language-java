module Language.Java.Syntax.VarDecl (ident) where

import Language.Java.Syntax (Ident, VarDecl (..))
import qualified Language.Java.Syntax.VarDeclId as VarDeclId (ident)

ident :: VarDecl p -> Ident
ident (VarDecl _ varDeclIdent _) = VarDeclId.ident varDeclIdent

