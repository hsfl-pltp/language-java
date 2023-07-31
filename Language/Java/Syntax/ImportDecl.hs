module Language.Java.Syntax.ImportDecl (classIdent) where

import Language.Java.Syntax (Ident, ImportDecl (..), Name (Name))

classIdent :: ImportDecl -> Maybe Ident
classIdent (ImportDecl _ False (Name _ idents) False) = Just (last idents)
classIdent _ = Nothing