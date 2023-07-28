module Language.Java.Syntax.VarDeclId (ident) where

import Language.Java.Syntax (Ident, VarDeclId (..))

ident :: VarDeclId -> Ident
ident (VarId idnt) = idnt
ident (VarDeclArray _ varDeclId) = ident varDeclId