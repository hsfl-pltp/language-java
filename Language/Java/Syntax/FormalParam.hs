module Language.Java.Syntax.FormalParam where

import Language.Java.Syntax (FormalParam (FormalParam), Ident)
import qualified Language.Java.Syntax.VarDeclId as VarDeclId

ident :: FormalParam p -> Ident
ident (FormalParam _ _ _ _ vardeclid) = VarDeclId.ident vardeclid