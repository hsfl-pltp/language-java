module Language.Java.Syntax.ClassDecl (ident) where

import Language.Java.Syntax (ClassDecl (..), Ident)

ident :: ClassDecl p -> Ident
ident (ClassDecl _ _ idnt _ _ _ _) = idnt
ident (RecordDecl _ _ idnt _ _ _ _) = idnt
ident (EnumDecl _ _ idnt _ _) = idnt
