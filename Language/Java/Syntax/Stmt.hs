module Language.Java.Syntax.Stmt
  ( hasNoSideEffect,
    isBasicFor,
    isBreak,
    isDo,
    isEnhancedFor,
    isWhile,
    isReturn,
    isSwitch,
  )
where

import Language.Java.Syntax (Stmt (..))

hasNoSideEffect :: Stmt p -> Bool
hasNoSideEffect (StmtBlock {}) = False
hasNoSideEffect (IfThen {}) = False
hasNoSideEffect (IfThenElse {}) = False
hasNoSideEffect (While {}) = False
hasNoSideEffect (BasicFor {}) = False
hasNoSideEffect (EnhancedFor {}) = False
hasNoSideEffect (Empty {}) = False
hasNoSideEffect (ExpStmt {}) = False
hasNoSideEffect (Assert {}) = False
hasNoSideEffect (Switch {}) = False
hasNoSideEffect (Do {}) = False
hasNoSideEffect (Break {}) = True
hasNoSideEffect (Continue {}) = True
hasNoSideEffect (Return {}) = True
hasNoSideEffect (Synchronized {}) = False
hasNoSideEffect (Throw {}) = False
hasNoSideEffect (Try {}) = False
hasNoSideEffect (Labeled _ _ stmt) = hasNoSideEffect stmt

isBreak :: Stmt p -> Bool
isBreak (Break {}) = True
isBreak _ = False

isWhile :: Stmt p -> Bool
isWhile (While {}) = True
isWhile _ = False

isDo :: Stmt p -> Bool
isDo (Do {}) = True
isDo _ = False

isBasicFor :: Stmt p -> Bool
isBasicFor (BasicFor {}) = True
isBasicFor _ = False

isEnhancedFor :: Stmt p -> Bool
isEnhancedFor (EnhancedFor {}) = True
isEnhancedFor _ = False

isReturn :: Stmt p -> Bool
isReturn (Return {}) = True
isReturn _ = False

isSwitch :: Stmt p -> Bool
isSwitch (Switch {}) = True
isSwitch _ = False
