module Language.Java.Transformer where

import Data.Bifunctor (Bifunctor (second))
import Language.Java.Syntax

class Transform a where
  analyze :: [Ident] -> a Parsed -> a Analyzed

instance Transform CompilationUnit where
  analyze scope (CompilationUnit mbPackageDecl importDecls typeDecls) =
    CompilationUnit
      (fmap (analyze scope) mbPackageDecl)
      (map (analyze scope) importDecls)
      (map (analyze scope) typeDecls)

instance Transform PackageDecl where
  analyze _ (PackageDecl name) = PackageDecl name

instance Transform ImportDecl where
  analyze _ (ImportDecl src onlyStatics name single) =
    ImportDecl src onlyStatics name single

instance Transform TypeDecl where
  analyze scope (ClassTypeDecl classDecl) = ClassTypeDecl (analyze scope classDecl)
  analyze scope (InterfaceTypeDecl interfaceDecl) = InterfaceTypeDecl (analyze scope interfaceDecl)

instance Transform ClassDecl where
  analyze scope (ClassDecl src modifiers ident typeParams mbRefType refTypes classBody) =
    ClassDecl
      src
      (map (analyze scope) modifiers)
      ident
      typeParams
      mbRefType
      refTypes
      (analyze scope classBody)
  analyze scope (RecordDecl src modifiers ident typeParams recordFieldDecls refTypes classBody) =
    RecordDecl
      src
      (map (analyze scope) modifiers)
      ident
      typeParams
      recordFieldDecls
      refTypes
      (analyze scope classBody)
  analyze scope (EnumDecl src modifiers ident refTypes enumBody) =
    EnumDecl
      src
      (map (analyze scope) modifiers)
      ident
      refTypes
      (analyze scope enumBody)

instance Transform InterfaceDecl where
  analyze scope (InterfaceDecl src kind modifiers ident typeParams refTypesExtends refTypesPermits interfaceBody) =
    InterfaceDecl
      src
      kind
      (map (analyze scope) modifiers)
      ident
      typeParams
      refTypesExtends
      refTypesPermits
      (analyze scope interfaceBody)

instance Transform Modifier where
  analyze _ (Public src) = Public src
  analyze _ Private = Private
  analyze _ Protected = Protected
  analyze _ (Abstract src) = Abstract src
  analyze _ Final = Final
  analyze _ Static = Static
  analyze _ StrictFP = StrictFP
  analyze _ Transient = Transient
  analyze _ Volatile = Volatile
  analyze _ Native = Native
  analyze scope (Annotation anno) = Annotation (analyze scope anno)
  analyze _ Synchronized_ = Synchronized_
  analyze _ Sealed = Sealed

instance Transform Annotation where
  analyze scope (NormalAnnotation span annName annkV) =
    NormalAnnotation
      span
      annName
      (map (second (analyze scope)) annkV)
  analyze scope (SingleElementAnnotation span annName annValue) =
    SingleElementAnnotation span annName (analyze scope annValue)
  analyze _ (MarkerAnnotation span annName) = MarkerAnnotation span annName

instance Transform ClassBody where
  analyze scope (ClassBody decls) = ClassBody (map (analyze scope) decls)

instance Transform EnumBody where
  analyze scope (EnumBody enumConstants decls) = EnumBody (map (analyze scope) enumConstants) (map (analyze scope) decls)

instance Transform InterfaceBody where
  analyze scope (InterfaceBody memberDecls) = InterfaceBody (map (analyze scope) memberDecls)

instance Transform ElementValue where
  analyze scope (EVVal varInit) = EVVal (analyze scope varInit)
  analyze scope (EVAnn anno) = EVAnn (analyze scope anno)

instance Transform Decl where
  analyze scope (MemberDecl memberDecl) = MemberDecl (analyze scope memberDecl)
  analyze scope (InitDecl static block) = InitDecl static (analyze scope block)

instance Transform EnumConstant where
  analyze scope (EnumConstant ident arguments mbClassBody) = EnumConstant ident (map (analyze scope) arguments) (fmap (analyze scope) mbClassBody)

instance Transform MemberDecl where
  analyze scope (FieldDecl span modifiers type_ varDecls) = FieldDecl span (map (analyze scope) modifiers) type_ (map (analyze scope) varDecls)
  analyze scope (MethodDecl span modifiers typeParams mbType ident formalParams exceptionTypes mbExp methodBody) =
    MethodDecl
      span
      (map (analyze scope) modifiers)
      typeParams
      mbType
      ident
      (map (analyze scope) formalParams)
      exceptionTypes
      (fmap (analyze scope) mbExp)
      (analyze scope methodBody)
  analyze scope (ConstructorDecl span modifiers typeParams ident formalParams exceptionTypes constructorBody) =
    ConstructorDecl
      span
      (map (analyze scope) modifiers)
      typeParams
      ident
      (map (analyze scope) formalParams)
      exceptionTypes
      (analyze scope constructorBody)
  analyze scope (MemberClassDecl classDecl) = MemberClassDecl (analyze scope classDecl)
  analyze scope (MemberInterfaceDecl interfaceDecl) = MemberInterfaceDecl (analyze scope interfaceDecl)

instance Transform VarInit where
  analyze scope (InitExp exp) = InitExp (analyze scope exp)
  analyze scope (InitArray arrayInit) = InitArray (analyze scope arrayInit)

instance Transform Block where
  analyze scope (Block blockstmts) = Block (map (analyze scope) blockstmts)

instance Transform BlockStmt where
  analyze scope (BlockStmt span stmt) = BlockStmt span (analyze scope stmt)
  analyze scope (LocalClass classDecl) = LocalClass (analyze scope classDecl)
  analyze scope (LocalVars span modifiers type_ varDecls) =
    LocalVars
      span
      (map (analyze scope) modifiers)
      type_
      (map (analyze scope) varDecls)

instance Transform Exp where
  analyze _ (Lit literal) = Lit literal
  analyze _ (ClassLit mbType) = ClassLit mbType
  analyze _ This = This
  analyze _ (ThisClass name) = ThisClass name
  analyze scope (InstanceCreation typeArgs typeDeclSpec arguments mbClassBody) =
    InstanceCreation
      typeArgs
      typeDeclSpec
      (map (analyze scope) arguments)
      (fmap (analyze scope) mbClassBody)
  analyze scope (QualInstanceCreation exp typeArgs ident arguments mbClassBody) =
    QualInstanceCreation
      (analyze scope exp)
      typeArgs
      ident
      (map (analyze scope) arguments)
      (fmap (analyze scope) mbClassBody)
  analyze scope (ArrayCreate type_ exps int) = ArrayCreate type_ (map (analyze scope) exps) int
  analyze scope (ArrayCreateInit type_ int arrayInit) = ArrayCreateInit type_ int (analyze scope arrayInit)
  analyze scope (FieldAccess fieldAccess) = FieldAccess (analyze scope fieldAccess)
  analyze scope (MethodInv methodInv) = MethodInv (analyze scope methodInv)
  analyze scope (ArrayAccess arrayIndex) = ArrayAccess (analyze scope arrayIndex)
  analyze _ (ExpName name) = ExpName name
  analyze scope (PostIncrement span exp) = PostIncrement span (analyze scope exp)
  analyze scope (PostDecrement span exp) = PostDecrement span (analyze scope exp)
  analyze scope (PreIncrement span exp) = PreIncrement span (analyze scope exp)
  analyze scope (PreDecrement span exp) = PreDecrement span (analyze scope exp)
  analyze scope (PrePlus exp) = PrePlus (analyze scope exp)
  analyze scope (PreMinus exp) = PreMinus (analyze scope exp)
  analyze scope (PreBitCompl exp) = PreBitCompl (analyze scope exp)
  analyze scope (PreNot exp) = PreNot (analyze scope exp)
  analyze scope (Cast type_ exp) = Cast type_ (analyze scope exp)
  analyze scope (BinOp exp1 op exp2) = BinOp (analyze scope exp1) op (analyze scope exp2)
  analyze scope (InstanceOf exp refType mbName) = InstanceOf (analyze scope exp) refType mbName
  analyze scope (Cond span exp1 exp2 exp3) = Cond span (analyze scope exp1) (analyze scope exp2) (analyze scope exp3)
  analyze scope (Assign span lhs assignOp exp) = Assign span (analyze scope lhs) assignOp (analyze scope exp)
  analyze scope (Lambda lambdaParams lambdaExp) = Lambda (analyze scope lambdaParams) (analyze scope lambdaExp)
  analyze _ (MethodRef name target) = MethodRef name target
  analyze scope (SwitchExp exp branches) = SwitchExp (analyze scope exp) (map (analyze scope) branches)

instance Transform FormalParam where
  analyze scope (FormalParam span modifiers type_ bool varDeclId) = FormalParam span (map (analyze scope) modifiers) type_ bool varDeclId

instance Transform VarDecl where
  analyze scope (VarDecl span varDeclId mbVarInit) = VarDecl span varDeclId (fmap (analyze scope) mbVarInit)

instance Transform MethodBody where
  analyze scope (MethodBody mbBlock) = MethodBody (fmap (analyze scope) mbBlock)

instance Transform ConstructorBody where
  analyze scope (ConstructorBody mbInv blockStmts) = ConstructorBody (fmap (analyze scope) mbInv) (map (analyze scope) blockStmts)

instance Transform ArrayInit where
  analyze scope (ArrayInit varInits) = ArrayInit (map (analyze scope) varInits)

instance Transform Stmt where
  analyze scope (StmtBlock block) = StmtBlock (analyze scope block)
  analyze scope (IfThen span exp stmt) = IfThen span (analyze scope exp) (analyze scope stmt)
  analyze scope (IfThenElse span exp stmt1 stmt2) = IfThenElse span (analyze scope exp) (analyze scope stmt1) (analyze scope stmt2)
  analyze scope (While span exp stmt) = While span (analyze scope exp) (analyze scope stmt)
  analyze scope (BasicFor span mbForInit mbExp mbExps stmt) =
    BasicFor
      span
      (fmap (analyze scope) mbForInit)
      (fmap (analyze scope) mbExp)
      (fmap (map (analyze scope)) mbExps)
      (analyze scope stmt)
  analyze scope (EnhancedFor span mods type_ ident exp stmt) = EnhancedFor span (map (analyze scope) mods) type_ ident (analyze scope exp) (analyze scope stmt)
  analyze _ Empty = Empty
  analyze scope (ExpStmt span exp) = ExpStmt span (analyze scope exp)
  analyze scope (Assert exp mbExp) = Assert (analyze scope exp) (fmap (analyze scope) mbExp)
  analyze scope (Switch switchStyle exp switchBlocks) = Switch switchStyle (analyze scope exp) (map (analyze scope) switchBlocks)
  analyze scope (Do span exp stmt) = Do span (analyze scope exp) (analyze scope stmt)
  analyze _ (Break span mbIdent) = Break span mbIdent
  analyze _ (Continue mbIdent) = Continue mbIdent
  analyze scope (Return span mbExp) = Return span (fmap (analyze scope) mbExp)
  analyze scope (Synchronized exp block) = Synchronized (analyze scope exp) (analyze scope block)
  analyze scope (Throw exp) = Throw (analyze scope exp)
  analyze scope (Try span tryResources block catches mbBlock) =
    Try
      span
      (map (analyze scope) tryResources)
      (analyze scope block)
      (map (analyze scope) catches)
      (fmap (analyze scope) mbBlock)
  analyze scope (Labeled ident stmt) = Labeled ident (analyze scope stmt)

instance Transform FieldAccess where
  analyze scope (PrimaryFieldAccess exp ident) = PrimaryFieldAccess (analyze scope exp) ident
  analyze _ (SuperFieldAccess ident) = SuperFieldAccess ident
  analyze _ (ClassFieldAccess name ident) = ClassFieldAccess name ident

instance Transform MethodInvocation where
  -- Todo : Classify name
  analyze scope (MethodCall mbName ident args) = MethodCall Nothing ident (map (analyze scope) args)
  analyze scope (PrimaryMethodCall exp refTypes ident args) = PrimaryMethodCall (analyze scope exp) refTypes ident (map (analyze scope) args)
  analyze scope (SuperMethodCall refTypes ident args) = SuperMethodCall refTypes ident (map (analyze scope) args)
  analyze scope (ClassMethodCall name refTypes ident args) = ClassMethodCall name refTypes ident (map (analyze scope) args)
  analyze scope (TypeMethodCall name refTypes ident args) = TypeMethodCall name refTypes ident (map (analyze scope) args)

instance Transform ArrayIndex where
  analyze scope (ArrayIndex exp exps) = ArrayIndex (analyze scope exp) (map (analyze scope) exps)

instance Transform Lhs where
  analyze _ (NameLhs name) = NameLhs name
  analyze scope (FieldLhs fieldAccess) = FieldLhs (analyze scope fieldAccess)
  analyze scope (ArrayLhs arrayIndex) = ArrayLhs (analyze scope arrayIndex)

instance Transform LambdaParams where
  analyze _ (LambdaSingleParam ident) = LambdaSingleParam ident
  analyze scope (LambdaFormalParams formalParams) = LambdaFormalParams (map (analyze scope) formalParams)
  analyze _ (LambdaInferredParams idents) = LambdaInferredParams idents

instance Transform LambdaExpression where
  analyze scope (LambdaExpression exp) = LambdaExpression (analyze scope exp)
  analyze scope (LambdaBlock block) = LambdaBlock (analyze scope block)

instance Transform ForInit where
  analyze scope (ForLocalVars modifiers type_ varDecls) = ForLocalVars (map (analyze scope) modifiers) type_ (map (analyze scope) varDecls)
  analyze scope (ForInitExps exps) = ForInitExps (map (analyze scope) exps)

instance Transform SwitchExpBranch where
  analyze scope (SwitchExpBranch label body) = SwitchExpBranch (analyze scope label) (analyze scope body)

instance Transform SwitchLabel where
  analyze scope (SwitchCase exps) = SwitchCase (map (analyze scope) exps)
  analyze _ Default = Default

instance Transform SwitchExpBranchBody where
  analyze scope (SwitchExpBranchExp exp) = SwitchExpBranchExp (analyze scope exp)
  analyze scope (SwitchExpBranchBlock blockStmts) = SwitchExpBranchBlock (map (analyze scope) blockStmts)

instance Transform ExplConstrInv where
  analyze scope (ThisInvoke refTypes args) = ThisInvoke refTypes (map (analyze scope) args)
  analyze scope (SuperInvoke refTypes args) = SuperInvoke refTypes (map (analyze scope) args)
  analyze scope (PrimarySuperInvoke exp refTypes args) = PrimarySuperInvoke (analyze scope exp) refTypes (map (analyze scope) args)

instance Transform SwitchBlock where
  analyze scope (SwitchBlock span switchLabel blockStmts) = SwitchBlock span (analyze scope switchLabel) (map (analyze scope) blockStmts)

instance Transform TryResource where
  analyze scope (TryResourceVarDecl resourceDecl) = TryResourceVarDecl (analyze scope resourceDecl)
  analyze _ (TryResourceVarAccess ident) = TryResourceVarAccess ident
  analyze scope (TryResourceQualAccess fieldAccess) = TryResourceQualAccess (analyze scope fieldAccess)

instance Transform Catch where
  analyze scope (Catch formalParam block) = Catch (analyze scope formalParam) (analyze scope block)

instance Transform ResourceDecl where
  analyze scope (ResourceDecl modifiers type_ varDeclId varInit) = ResourceDecl (map (analyze scope) modifiers) type_ varDeclId (analyze scope varInit)