module Language.Java.Syntax.TryResource (resourceDecl, resourceDeclIdent) where

import Language.Java.Syntax (Ident, ResourceDecl, TryResource (TryResourceVarDecl))
import qualified Language.Java.Syntax.ResourceDecl as ResourceDecl

resourceDecl :: TryResource p -> Maybe (ResourceDecl p)
resourceDecl (TryResourceVarDecl resourcedecl) = Just resourcedecl
resourceDecl _ = Nothing

resourceDeclIdent :: TryResource p -> Maybe Ident
resourceDeclIdent tr = fmap ResourceDecl.ident (resourceDecl tr)