module Language.Java.Syntax.VarInit where

import Language.Java.Syntax (ArrayInit (ArrayInit), VarInit (..))
import qualified Language.Java.Syntax.Exp as Exp

hasNoSideEffect :: VarInit p -> Bool
hasNoSideEffect (InitExp expr) = Exp.hasNoSideEffect expr
hasNoSideEffect (InitArray (ArrayInit _ varInits)) = all hasNoSideEffect varInits
