module Language.Java.Syntax.Exp
  ( isCast,
    isPostInc,
    isPostDec,
    isArrayCreate,
    isIncstanceCreation,
    isQualInstanceCreation,
    hasNoSideEffect,
  )
where

import Language.Java.Syntax (Exp (..))

isCast :: Exp p -> Bool
isCast (Cast {}) = True
isCast _ = False

isPostInc :: Exp p -> Bool
isPostInc (PostIncrement _ _) = True
isPostInc _ = False

isPostDec :: Exp p -> Bool
isPostDec (PostDecrement _ _) = True
isPostDec _ = False

isArrayCreate :: Exp p -> Bool
isArrayCreate (ArrayCreate {}) = True
isArrayCreate _ = False

isIncstanceCreation :: Exp p -> Bool
isIncstanceCreation (InstanceCreation {}) = True
isIncstanceCreation _ = False

isQualInstanceCreation :: Exp p -> Bool
isQualInstanceCreation (QualInstanceCreation {}) = True
isQualInstanceCreation _ = False

hasNoSideEffect :: Exp p -> Bool
hasNoSideEffect (Lit _) = True
hasNoSideEffect (ClassLit _ _) = False
hasNoSideEffect (This _) = False
hasNoSideEffect (ThisClass {}) = False
hasNoSideEffect (InstanceCreation {}) = False
hasNoSideEffect (QualInstanceCreation {}) = False
hasNoSideEffect (ArrayCreate {}) = False
hasNoSideEffect (ArrayCreateInit {}) = False
hasNoSideEffect (FieldAccess {}) = True
hasNoSideEffect (MethodInv {}) = False
hasNoSideEffect (ArrayAccess {}) = False
hasNoSideEffect (ExpName {}) = True
hasNoSideEffect (PostIncrement _ _) = False
hasNoSideEffect (PostDecrement _ _) = False
hasNoSideEffect (PreIncrement _ _) = False
hasNoSideEffect (PreDecrement _ _) = False
hasNoSideEffect (PrePlus {}) = False
hasNoSideEffect (PreMinus {}) = False
hasNoSideEffect (PreBitCompl {}) = False
hasNoSideEffect (PreNot {}) = False
hasNoSideEffect (SwitchExp {}) = False
hasNoSideEffect (Cast {}) = False
hasNoSideEffect (BinOp _ leftExp _ rightExp) = hasNoSideEffect leftExp && hasNoSideEffect rightExp
hasNoSideEffect (InstanceOf {}) = False
hasNoSideEffect (Cond {}) = False
hasNoSideEffect (Assign {}) = False
hasNoSideEffect (Lambda {}) = False
hasNoSideEffect (MethodRef {}) = False
