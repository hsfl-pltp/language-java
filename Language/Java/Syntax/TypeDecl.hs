module Language.Java.Syntax.TypeDecl (classDecl) where

import Language.Java.Syntax (ClassDecl, TypeDecl (ClassTypeDecl))

classDecl :: TypeDecl p -> Maybe (ClassDecl p)
classDecl (ClassTypeDecl classdecl) = Just classdecl
classDecl _ = Nothing