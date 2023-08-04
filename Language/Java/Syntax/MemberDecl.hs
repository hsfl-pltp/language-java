module Language.Java.Syntax.MemberDecl (classDecl, fields, formalParams) where

import Language.Java.Syntax (ClassDecl, FormalParam, MemberDecl (..), VarDecl)

classDecl :: MemberDecl p -> Maybe (ClassDecl p)
classDecl (MemberClassDecl classdecl) = Just classdecl
classDecl _ = Nothing

fields :: MemberDecl p -> Maybe [VarDecl p]
fields (FieldDecl _ _ _ vardecls) = Just vardecls
fields _ = Nothing

formalParams :: MemberDecl p -> Maybe [FormalParam p]
formalParams (MethodDecl _ _ _ _ _ params _ _ _) = Just params
formalParams (ConstructorDecl _ _ _ _ params _ _) = Just params
formalParams _ = Nothing