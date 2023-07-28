module Language.Java.Syntax.Decl (memberDecl) where

import Language.Java.Syntax (Decl (..), MemberDecl)

memberDecl :: Decl p -> Maybe (MemberDecl p)
memberDecl (MemberDecl memberdecl) = Just memberdecl
memberDecl _ = Nothing