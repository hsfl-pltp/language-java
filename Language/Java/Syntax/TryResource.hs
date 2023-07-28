module Language.Java.Syntax.TryResource (resourceDecl) where

import Language.Java.Syntax (ResourceDecl, TryResource (TryResourceVarDecl))

resourceDecl :: TryResource p -> Maybe (ResourceDecl p)
resourceDecl (TryResourceVarDecl resourcedecl) = Just resourcedecl
resourceDecl _ = Nothing
