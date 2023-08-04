module Language.Java.Syntax.ResourceDecl (ident) where

import Language.Java.Syntax (Ident, ResourceDecl (..))
import qualified Language.Java.Syntax.VarDeclId as VarDeclId

ident :: ResourceDecl p -> Ident
ident (ResourceDecl _ _ varDeclId _) = VarDeclId.ident varDeclId