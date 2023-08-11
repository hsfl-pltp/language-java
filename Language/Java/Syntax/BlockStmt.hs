module Language.Java.Syntax.BlockStmt (hasNoSideEffect) where

import Language.Java.Syntax (BlockStmt (..))
import qualified Language.Java.Syntax.Stmt as Stmt
import qualified Language.Java.Syntax.VarDecl as VarDecl

hasNoSideEffect :: BlockStmt p -> Bool
hasNoSideEffect (BlockStmt stmt) = Stmt.hasNoSideEffect stmt
hasNoSideEffect (LocalClass _) = False
hasNoSideEffect (LocalVars _ _ _ varDecls) = all VarDecl.hasNoSideEffect varDecls
