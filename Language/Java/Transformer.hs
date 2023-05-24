{-# LANGUAGE LambdaCase #-}

module Language.Java.Transformer (analyze, IdentCollection (..)) where

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe (mapMaybe)
import Language.Java.Syntax

class Transform a where
  analyze :: IdentCollection -> a Parsed -> a Analyzed

-- | data type used to collect the identifiers that are currently in scope
data IdentCollection = IdentCollection
  { fields :: [Ident],
    formalParams :: [Ident],
    localVars :: [Ident]
  }

addFields :: IdentCollection -> ClassDecl Parsed -> IdentCollection
addFields identCollection classDecl = identCollection {fields = fieldsFromClassDecl classDecl ++ fields identCollection}

addFormalParams :: IdentCollection -> MemberDecl Parsed -> IdentCollection
addFormalParams identCollection idents = identCollection {formalParams = methodParameters idents ++ formalParams identCollection}

addLocalVars :: IdentCollection -> [Ident] -> IdentCollection
addLocalVars identCollection idents = identCollection {localVars = idents ++ localVars identCollection}

instance Transform ClassDecl where
  analyze scope classDecl@(ClassDecl src modifiers ident typeParams mbRefType refTypes classBody) =
    let newScope = addFields scope classDecl
     in ClassDecl
          src
          (map (analyze newScope) modifiers)
          ident
          typeParams
          mbRefType
          refTypes
          (analyze newScope classBody)
  analyze scope recordDecl@(RecordDecl src modifiers ident typeParams recordFieldDecls refTypes classBody) =
    let newScope = addFields scope recordDecl
     in RecordDecl
          src
          (map (analyze newScope) modifiers)
          ident
          typeParams
          recordFieldDecls
          refTypes
          (analyze newScope classBody)
  analyze scope enumDecl@(EnumDecl src modifiers ident refTypes enumBody) =
    let newScope = addFields scope enumDecl
     in EnumDecl
          src
          (map (analyze newScope) modifiers)
          ident
          refTypes
          (analyze newScope enumBody)

instance Transform MemberDecl where
  analyze scope (FieldDecl span modifiers type_ varDecls) = FieldDecl span (map (analyze scope) modifiers) type_ (map (analyze scope) varDecls)
  analyze scope methodDecl@(MethodDecl span modifiers typeParams mbType ident formalParams exceptionTypes mbExp methodBody) =
    MethodDecl
      span
      (map (analyze scope) modifiers)
      typeParams
      mbType
      ident
      (map (analyze scope) formalParams)
      exceptionTypes
      (fmap (analyze scope) mbExp)
      (analyze (addFormalParams scope methodDecl) methodBody)
  analyze scope constructorDecl@(ConstructorDecl span modifiers typeParams ident formalParams exceptionTypes constructorBody) =
    ConstructorDecl
      span
      (map (analyze scope) modifiers)
      typeParams
      ident
      (map (analyze scope) formalParams)
      exceptionTypes
      (analyze (addFormalParams scope constructorDecl) constructorBody)
  analyze scope (MemberClassDecl classDecl) = MemberClassDecl (analyze scope classDecl)
  analyze scope (MemberInterfaceDecl interfaceDecl) = MemberInterfaceDecl (analyze scope interfaceDecl)

instance Transform Catch where
  analyze scope catch@(Catch formalParam block) =
    Catch
      (analyze scope formalParam)
      (analyze (addLocalVars scope (exceptionParameter catch)) block)

instance Transform Block where
  analyze scope block@(Block blockstmts) = Block (map (analyze (addLocalVars scope (localVarsInBlock block))) blockstmts)

instance Transform Stmt where
  analyze scope (StmtBlock block) = StmtBlock (analyze scope block)
  analyze scope (IfThen span exp stmt) = IfThen span (analyze scope exp) (analyze scope stmt)
  analyze scope (IfThenElse span exp stmt1 stmt2) = IfThenElse span (analyze scope exp) (analyze scope stmt1) (analyze scope stmt2)
  analyze scope (While span exp stmt) = While span (analyze scope exp) (analyze scope stmt)
  analyze scope basicFor@(BasicFor span mbForInit mbExp mbExps stmt) =
    let newScope = addLocalVars scope (localVarsInStmt basicFor)
     in BasicFor
          span
          (fmap (analyze scope) mbForInit)
          (fmap (analyze newScope) mbExp)
          (fmap (map (analyze newScope)) mbExps)
          (analyze newScope stmt)
  analyze scope enhancedFor@(EnhancedFor span mods type_ ident exp stmt) =
    EnhancedFor
      span
      (map (analyze scope) mods)
      type_
      ident
      (analyze scope exp)
      (analyze (addLocalVars scope (localVarsInStmt enhancedFor)) stmt)
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
  analyze scope tryStmt@(Try span tryResources block catches mbBlock) =
    Try
      span
      (map (analyze scope) tryResources)
      (analyze (addLocalVars scope (localVarsInStmt tryStmt)) block)
      (map (analyze scope) catches)
      (fmap (analyze scope) mbBlock)
  analyze scope (Labeled ident stmt) = Labeled ident (analyze scope stmt)

-- classification of MethodInvocation name
instance Transform MethodInvocation where
  -- Todo : classify multipart name
  analyze scope (MethodCall Nothing ident args) = MethodCall Nothing ident (map (analyze scope) args)
  analyze scope (MethodCall (Just name@(Name _ [singleId])) ident args) = MethodCall (Just (classifyName singleId scope name)) ident (map (analyze scope) args)
  analyze scope (MethodCall mbName ident args) = MethodCall Nothing ident (map (analyze scope) args)
  analyze scope (PrimaryMethodCall exp refTypes ident args) = PrimaryMethodCall (analyze scope exp) refTypes ident (map (analyze scope) args)
  analyze scope (SuperMethodCall refTypes ident args) = SuperMethodCall refTypes ident (map (analyze scope) args)
  analyze scope (ClassMethodCall name refTypes ident args) = ClassMethodCall name refTypes ident (map (analyze scope) args)
  analyze scope (TypeMethodCall name refTypes ident args) = TypeMethodCall name refTypes ident (map (analyze scope) args)

classifyName :: Ident -> IdentCollection -> Name -> ClassifiedName
classifyName ident (IdentCollection cfields formal locals) = if any (eq IgnoreSourceSpan ident) (cfields ++ formal ++ locals) then ExpressionName else TypeName

-- helper functions

identFromVarDeclId :: VarDeclId -> Ident
identFromVarDeclId (VarId ident) = ident
identFromVarDeclId (VarDeclArray _ varDeclId) = identFromVarDeclId varDeclId

-- class fields from ClassDecl
fieldsFromClassDecl :: ClassDecl p -> [Ident]
fieldsFromClassDecl (ClassDecl _ _ _ _ _ _ (ClassBody decls)) = identsfromDecls decls
fieldsFromClassDecl (RecordDecl _ _ _ _ _ _ (ClassBody decls)) = identsfromDecls decls
fieldsFromClassDecl (EnumDecl _ _ _ _ (EnumBody _ decls)) = identsfromDecls decls

identsfromDecls :: [Decl p] -> [Ident]
identsfromDecls decls = do
  map
    (identFromVarDeclId . (\(VarDecl _ id _) -> id))
    ( concatMap
        ( \case
            (MemberDecl (FieldDecl _ _ _ vars)) -> vars
            _ -> []
        )
        decls
    )

-- | ident of exception parameter from Catch
exceptionParameter :: Catch p -> [Ident]
exceptionParameter (Catch (FormalParam _ _ _ _ vardeclId) _) = [identFromVarDeclId vardeclId]

-- | idents of formal parameters of method and constructor declaration
methodParameters :: MemberDecl p -> [Ident]
methodParameters (MethodDecl _ _ _ _ _ params _ _ _) = identsFromParams params
methodParameters (ConstructorDecl _ _ _ _ params _ _) = identsFromParams params
methodParameters _ = []

identsFromParams :: [FormalParam p] -> [Ident]
identsFromParams = map (identFromVarDeclId . (\(FormalParam _ _ _ _ vardeclId) -> vardeclId))

-- | idents of local variables from block
localVarsInBlock :: Block p -> [Ident]
localVarsInBlock (Block blockstmts) = concatMap identsFromBlockStmt blockstmts
  where
    identsFromBlockStmt (LocalVars _ _ _ varDecls) = map (identFromVarDeclId . (\(VarDecl _ id _) -> id)) varDecls
    identsFromBlockStmt _ = []

-- | idents of local variables from basicFor, enhancedFor and tryWithResource
localVarsInStmt :: Stmt p -> [Ident]
localVarsInStmt (BasicFor _ (Just (ForLocalVars _ _ varDecls)) _ _ _) = map (identFromVarDeclId . (\(VarDecl _ id _) -> id)) varDecls
localVarsInStmt (EnhancedFor _ _ _ ident _ _) = [ident]
localVarsInStmt (Try _ tryResources _ _ _) = mapMaybe identFromTryResource tryResources
  where
    identFromTryResource (TryResourceVarDecl (ResourceDecl _ _ vardDeclId _)) = Just (identFromVarDeclId vardDeclId)
    identFromTryResource _ = Nothing
localVarsInStmt _ = []

-- boiler plate cases

instance Transform CompilationUnit where
  analyze scope (CompilationUnit mbPackageDecl importDecls typeDecls) =
    CompilationUnit
      mbPackageDecl
      importDecls
      (map (analyze scope) typeDecls)

instance Transform TypeDecl where
  analyze scope (ClassTypeDecl classDecl) = ClassTypeDecl (analyze scope classDecl)
  analyze scope (InterfaceTypeDecl interfaceDecl) = InterfaceTypeDecl (analyze scope interfaceDecl)

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

instance Transform VarInit where
  analyze scope (InitExp exp) = InitExp (analyze scope exp)
  analyze scope (InitArray arrayInit) = InitArray (analyze scope arrayInit)

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

instance Transform FieldAccess where
  analyze scope (PrimaryFieldAccess exp ident) = PrimaryFieldAccess (analyze scope exp) ident
  analyze _ (SuperFieldAccess ident) = SuperFieldAccess ident
  analyze _ (ClassFieldAccess name ident) = ClassFieldAccess name ident

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

instance Transform ResourceDecl where
  analyze scope (ResourceDecl modifiers type_ varDeclId varInit) = ResourceDecl (map (analyze scope) modifiers) type_ varDeclId (analyze scope varInit)