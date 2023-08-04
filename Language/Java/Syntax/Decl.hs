module Language.Java.Syntax.Decl (memberDecl, classDecl, fieldIdents) where

import Language.Java.Syntax (ClassDecl, Decl (..), Ident, MemberDecl)
import qualified Language.Java.Syntax.MemberDecl as MemberDecl
import qualified Language.Java.Syntax.VarDecl as VarDecl

memberDecl :: Decl p -> Maybe (MemberDecl p)
memberDecl (MemberDecl memberdecl) = Just memberdecl
memberDecl _ = Nothing

classDecl :: Decl p -> Maybe (ClassDecl p)
classDecl decl =
  memberDecl decl
    >>= MemberDecl.classDecl

fieldIdents :: Decl p -> Maybe [Ident]
fieldIdents decl =
  fmap
    (map VarDecl.ident)
    ( memberDecl decl
        >>= MemberDecl.fields
    )