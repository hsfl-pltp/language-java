{-# LANGUAGE LambdaCase #-}

module Language.Java.Transformer (transformCompilationUnitToAnalyzed) where

import Data.Bifunctor (Bifunctor (second))
import Data.List (find)
import Data.Maybe (mapMaybe)
import Language.Java.Syntax

-- | Entry point for transforming a compilationunit from parsed to analyzed and classifying all methodinvoker names
transformCompilationUnitToAnalyzed :: CompilationUnit Parsed -> CompilationUnit Analyzed
transformCompilationUnitToAnalyzed = transformToAnalyzed (IdentCollection [] [] [] [] [])

-- | data type used to pass down class structure in the file
data ClassTree = ClassTree
  { classFields :: [Ident],
    childClasses :: [ClassTree],
    ident :: Ident
  }

isClassTreeField :: Ident -> ClassTree -> Bool
isClassTreeField idnt classTree = any (eq IgnoreSourceSpan idnt) (classFields classTree)

isClassTreeChildClass :: Ident -> ClassTree -> Bool
isClassTreeChildClass idnt classTree = any (eq IgnoreSourceSpan idnt . ident) (childClasses classTree)

isClassTreeByIdent :: Ident -> ClassTree -> Bool
isClassTreeByIdent idnt (ClassTree _ _ ctIdnt) = eq IgnoreSourceSpan idnt ctIdnt

classTreeFromClassDecl :: ClassDecl p -> ClassTree
classTreeFromClassDecl (ClassDecl _ _ idnt _ _ _ (ClassBody decls)) =
  ClassTree
    (fieldIdentsFromDecls decls)
    (map classTreeFromClassDecl (classDeclsFromDecls decls))
    idnt
classTreeFromClassDecl (RecordDecl _ _ idnt _ _ _ (ClassBody decls)) =
  ClassTree
    (fieldIdentsFromDecls decls)
    (map classTreeFromClassDecl (classDeclsFromDecls decls))
    idnt
classTreeFromClassDecl (EnumDecl _ _ idnt _ (EnumBody cons decls)) =
  ClassTree
    (fieldIdentsFromDecls decls ++ enumConstantIdentsFromEC cons)
    (map classTreeFromClassDecl (classDeclsFromDecls decls))
    idnt

-- | data type used to collect the identifiers that are currently in scope
data IdentCollection = IdentCollection
  { fields :: [Ident],
    formalParams :: [Ident],
    localVars :: [Ident],
    typeNameIdents :: [Ident],
    classTrees :: [ClassTree]
  }

addFieldIdents :: [Ident] -> IdentCollection -> IdentCollection
addFieldIdents idents identCollection = identCollection {fields = idents ++ fields identCollection}

addFormalParameterIdents :: [Ident] -> IdentCollection -> IdentCollection
addFormalParameterIdents idents identCollection = identCollection {formalParams = idents ++ formalParams identCollection}

addLocalVarIdents :: [Ident] -> IdentCollection -> IdentCollection
addLocalVarIdents idents identCollection = identCollection {localVars = idents ++ localVars identCollection}

addTypeVarIdents :: [Ident] -> IdentCollection -> IdentCollection
addTypeVarIdents idents identCollection = identCollection {typeNameIdents = idents ++ typeNameIdents identCollection}

addClassTrees :: [ClassTree] -> IdentCollection -> IdentCollection
addClassTrees classTreesNew identCollection = identCollection {classTrees = classTreesNew ++ classTrees identCollection}

isInExpressionNameList :: Ident -> IdentCollection -> Bool
isInExpressionNameList idnt (IdentCollection cfields fp vars _ _) = any (eq IgnoreSourceSpan idnt) (cfields ++ fp ++ vars)

isInTypeNameList :: Ident -> IdentCollection -> Bool
isInTypeNameList idnt (IdentCollection _ _ _ typeVars _) = any (eq IgnoreSourceSpan idnt) typeVars

classifyName :: Ident -> IdentCollection -> Name -> ClassifiedName
classifyName idnt identCollection
  | idnt `isInExpressionNameList` identCollection = ExpressionName
  | idnt `isInTypeNameList` identCollection = TypeName
  | otherwise = Unknown

classifyQualifiedName :: [Ident] -> IdentCollection -> Name -> ClassifiedName
classifyQualifiedName [] _ = Unknown
classifyQualifiedName (idnt : rest) ic
  | idnt `isInExpressionNameList` ic = ExpressionName
  | any (isClassTreeByIdent idnt) (classTrees ic) =
      continueClassification (find (isClassTreeByIdent idnt) (classTrees ic)) rest
  | otherwise = Unknown

continueClassification :: Maybe ClassTree -> [Ident] -> Name -> ClassifiedName
continueClassification (Just classTree) idnts = classifyIdentFromTree idnts classTree
continueClassification _ _ = Unknown

classifyIdentFromTree :: [Ident] -> ClassTree -> Name -> ClassifiedName
classifyIdentFromTree [] _ = TypeName
classifyIdentFromTree (idnt : rest) classTree
  | isClassTreeField idnt classTree = ExpressionName
  | isClassTreeChildClass idnt classTree =
      continueClassification (find (isClassTreeByIdent idnt) (childClasses classTree)) rest
  | otherwise = Unknown

class AnalyzedTransformer a where
  transformToAnalyzed :: IdentCollection -> a Parsed -> a Analyzed

instance AnalyzedTransformer MethodInvocation where
  -- calling the method with its unqualified name, bar()
  transformToAnalyzed scope (MethodCall Nothing idnt args) = MethodCall Nothing idnt (map (transformToAnalyzed scope) args)
  -- single ident in name, foo.bar()
  transformToAnalyzed scope (MethodCall (Just name@(Name _ [singleId])) idnt args) = MethodCall (Just (classifyName singleId scope name)) idnt (map (transformToAnalyzed scope) args)
  -- multiple idents in name, math.foo.bar()
  transformToAnalyzed scope (MethodCall (Just name@(Name _ idents)) idnt args) = MethodCall (Just (classifyQualifiedName idents scope name)) idnt (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (PrimaryMethodCall expr refTypes idnt args) = PrimaryMethodCall (transformToAnalyzed scope expr) refTypes idnt (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (SuperMethodCall refTypes idnt args) = SuperMethodCall refTypes idnt (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (ClassMethodCall name refTypes idnt args) = ClassMethodCall name refTypes idnt (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (TypeMethodCall name refTypes idnt args) = TypeMethodCall name refTypes idnt (map (transformToAnalyzed scope) args)

instance AnalyzedTransformer CompilationUnit where
  transformToAnalyzed scope (CompilationUnit mbPackageDecl importDecls typeDecls) =
    let newscope = (addClassTrees (map classTreeFromClassDecl (classDeclsFromTypeDecls typeDecls)) . addTypeVarIdents (classIdentsFromTypeDecls typeDecls ++ classIdentsFromImports importDecls)) scope
     in CompilationUnit
          mbPackageDecl
          importDecls
          (map (transformToAnalyzed newscope) typeDecls)

instance AnalyzedTransformer ClassDecl where
  transformToAnalyzed scope (ClassDecl src modifiers idnt typeParams mbRefType refTypes classBody@(ClassBody decls)) =
    let newScope = (addClassTrees (map classTreeFromClassDecl (classDeclsFromDecls decls)) . addTypeVarIdents (idnt : classIdentsFromDecls decls) . addFieldIdents (fieldIdentsFromDecls decls)) scope
     in ClassDecl
          src
          (map (transformToAnalyzed scope) modifiers)
          idnt
          typeParams
          mbRefType
          refTypes
          (transformToAnalyzed newScope classBody)
  transformToAnalyzed scope (RecordDecl src modifiers idnt typeParams recordFieldDecls refTypes classBody@(ClassBody decls)) =
    let newScope = (addClassTrees (map classTreeFromClassDecl (classDeclsFromDecls decls)) . addTypeVarIdents (idnt : classIdentsFromDecls decls) . addFieldIdents (fieldIdentsFromDecls decls)) scope
     in RecordDecl
          src
          (map (transformToAnalyzed scope) modifiers)
          idnt
          typeParams
          recordFieldDecls
          refTypes
          (transformToAnalyzed newScope classBody)
  transformToAnalyzed scope (EnumDecl src modifiers idnt refTypes enumBody@(EnumBody cons decls)) =
    let newScope = (addClassTrees (map classTreeFromClassDecl (classDeclsFromDecls decls)) . addTypeVarIdents (idnt : classIdentsFromDecls decls) . addFieldIdents (fieldIdentsFromDecls decls ++ enumConstantIdentsFromEC cons)) scope
     in EnumDecl
          src
          (map (transformToAnalyzed scope) modifiers)
          idnt
          refTypes
          (transformToAnalyzed newScope enumBody)

instance AnalyzedTransformer MemberDecl where
  transformToAnalyzed scope (FieldDecl srcspan modifiers type_ varDecls) =
    FieldDecl srcspan (map (transformToAnalyzed scope) modifiers) type_ (map (transformToAnalyzed scope) varDecls)
  transformToAnalyzed scope methodDecl@(MethodDecl srcspan modifiers typeParams mbType idnt fp exceptionTypes mbExp methodBody) =
    MethodDecl
      srcspan
      (map (transformToAnalyzed scope) modifiers)
      typeParams
      mbType
      idnt
      (map (transformToAnalyzed scope) fp)
      exceptionTypes
      (fmap (transformToAnalyzed scope) mbExp)
      (transformToAnalyzed (addFormalParameterIdents (methodParameterIdents methodDecl) scope) methodBody)
  transformToAnalyzed scope constructorDecl@(ConstructorDecl srcspan modifiers typeParams idnt fp exceptionTypes constructorBody) =
    ConstructorDecl
      srcspan
      (map (transformToAnalyzed scope) modifiers)
      typeParams
      idnt
      (map (transformToAnalyzed scope) fp)
      exceptionTypes
      (transformToAnalyzed (addFormalParameterIdents (methodParameterIdents constructorDecl) scope) constructorBody)
  transformToAnalyzed scope (MemberClassDecl classDecl) = MemberClassDecl (transformToAnalyzed scope classDecl)
  transformToAnalyzed scope (MemberInterfaceDecl interfaceDecl) = MemberInterfaceDecl (transformToAnalyzed scope interfaceDecl)

instance AnalyzedTransformer Catch where
  transformToAnalyzed scope catch@(Catch formalParam block) =
    Catch
      (transformToAnalyzed scope formalParam)
      (transformToAnalyzed (addFormalParameterIdents (identFromExceptionParameter catch) scope) block)

instance AnalyzedTransformer Block where
  transformToAnalyzed scope (Block blockstmts) = Block (transformBlockStmtsToAnalyzed scope blockstmts)

transformBlockStmtsToAnalyzed :: IdentCollection -> [BlockStmt Parsed] -> [BlockStmt Analyzed]
transformBlockStmtsToAnalyzed _ [] = []
transformBlockStmtsToAnalyzed scope (blckStmt@(LocalVars _ _ _ varDecls) : rest) =
  let newscope = addLocalVarIdents (map (identFromVarDeclId . (\(VarDecl _ varId _) -> varId)) varDecls) scope
   in transformToAnalyzed newscope blckStmt : transformBlockStmtsToAnalyzed newscope rest
transformBlockStmtsToAnalyzed scope (blckStmt@(LocalClass classDecl) : rest) =
  let newscope = addTypeVarIdents [identFromClass classDecl] scope
   in transformToAnalyzed newscope blckStmt : transformBlockStmtsToAnalyzed newscope rest
transformBlockStmtsToAnalyzed scope (blckStmt : rest) = transformToAnalyzed scope blckStmt : transformBlockStmtsToAnalyzed scope rest

instance AnalyzedTransformer Stmt where
  transformToAnalyzed scope (StmtBlock block) = StmtBlock (transformToAnalyzed scope block)
  transformToAnalyzed scope (IfThen srcspan expr stmt) = IfThen srcspan (transformToAnalyzed scope expr) (transformToAnalyzed scope stmt)
  transformToAnalyzed scope (IfThenElse srcspan expr stmt1 stmt2) = IfThenElse srcspan (transformToAnalyzed scope expr) (transformToAnalyzed scope stmt1) (transformToAnalyzed scope stmt2)
  transformToAnalyzed scope (While srcspan expr stmt) = While srcspan (transformToAnalyzed scope expr) (transformToAnalyzed scope stmt)
  transformToAnalyzed scope (BasicFor srcspan mbForInit mbExp mbExps stmt) =
    let newScope = addLocalVarIdents (localVarsIdentsFromBasicForInit mbForInit) scope
     in BasicFor
          srcspan
          (fmap (transformToAnalyzed scope) mbForInit)
          (fmap (transformToAnalyzed newScope) mbExp)
          (fmap (map (transformToAnalyzed newScope)) mbExps)
          (transformToAnalyzed newScope stmt)
  transformToAnalyzed scope (EnhancedFor srcspan mods type_ idnt expr stmt) =
    EnhancedFor
      srcspan
      (map (transformToAnalyzed scope) mods)
      type_
      idnt
      (transformToAnalyzed scope expr)
      (transformToAnalyzed (addLocalVarIdents [idnt] scope) stmt)
  transformToAnalyzed _ Empty = Empty
  transformToAnalyzed scope (ExpStmt srcspan expr) = ExpStmt srcspan (transformToAnalyzed scope expr)
  transformToAnalyzed scope (Assert expr mbExp) = Assert (transformToAnalyzed scope expr) (fmap (transformToAnalyzed scope) mbExp)
  transformToAnalyzed scope (Switch switchStyle expr switchBlocks) = Switch switchStyle (transformToAnalyzed scope expr) (map (transformToAnalyzed scope) switchBlocks)
  transformToAnalyzed scope (Do srcspan expr stmt) = Do srcspan (transformToAnalyzed scope expr) (transformToAnalyzed scope stmt)
  transformToAnalyzed _ (Break srcspan mbIdent) = Break srcspan mbIdent
  transformToAnalyzed _ (Continue mbIdent) = Continue mbIdent
  transformToAnalyzed scope (Return srcspan mbExp) = Return srcspan (fmap (transformToAnalyzed scope) mbExp)
  transformToAnalyzed scope (Synchronized expr block) = Synchronized (transformToAnalyzed scope expr) (transformToAnalyzed scope block)
  transformToAnalyzed scope (Throw expr) = Throw (transformToAnalyzed scope expr)
  transformToAnalyzed scope (Try srcspan tryResources block catches mbBlock) =
    Try
      srcspan
      (map (transformToAnalyzed scope) tryResources)
      (transformToAnalyzed (addLocalVarIdents (localVarIdentsFromTryResources tryResources) scope) block)
      (map (transformToAnalyzed scope) catches)
      (fmap (transformToAnalyzed scope) mbBlock)
  transformToAnalyzed scope (Labeled idnt stmt) = Labeled idnt (transformToAnalyzed scope stmt)

-- helper functions

classDeclsFromTypeDecls :: [TypeDecl p] -> [ClassDecl p]
classDeclsFromTypeDecls =
  mapMaybe
    ( \case
        (ClassTypeDecl classDecl) -> Just classDecl
        _ -> Nothing
    )

classDeclsFromDecls :: [Decl p] -> [ClassDecl p]
classDeclsFromDecls =
  mapMaybe
    ( \case
        (MemberDecl (MemberClassDecl classDecl)) -> Just classDecl
        _ -> Nothing
    )

enumConstantIdentsFromEC :: [EnumConstant p] -> [Ident]
enumConstantIdentsFromEC = map (\(EnumConstant idnt _ _) -> idnt)

classIdentsFromTypeDecls :: [TypeDecl p] -> [Ident]
classIdentsFromTypeDecls =
  mapMaybe
    ( \case
        (ClassTypeDecl classDecl) -> Just (identFromClass classDecl)
        _ -> Nothing
    )

classIdentsFromImports :: [ImportDecl] -> [Ident]
classIdentsFromImports =
  mapMaybe
    ( \case
        (ImportDecl _ False (Name _ idents) False) -> Just (last idents)
        _ -> Nothing
    )

identFromVarDeclId :: VarDeclId -> Ident
identFromVarDeclId (VarId idnt) = idnt
identFromVarDeclId (VarDeclArray _ varDeclId) = identFromVarDeclId varDeclId

fieldIdentsFromDecls :: [Decl p] -> [Ident]
fieldIdentsFromDecls decls = do
  map
    (identFromVarDeclId . (\(VarDecl _ varId _) -> varId))
    ( concatMap
        ( \case
            (MemberDecl (FieldDecl _ _ _ vars)) -> vars
            _ -> []
        )
        decls
    )

classIdentsFromDecls :: [Decl p] -> [Ident]
classIdentsFromDecls =
  mapMaybe
    ( \case
        (MemberDecl (MemberClassDecl classDecl)) -> Just (identFromClass classDecl)
        _ -> Nothing
    )

identFromClass :: ClassDecl p -> Ident
identFromClass (ClassDecl _ _ idnt _ _ _ _) = idnt
identFromClass (RecordDecl _ _ idnt _ _ _ _) = idnt
identFromClass (EnumDecl _ _ idnt _ _) = idnt

identFromExceptionParameter :: Catch p -> [Ident]
identFromExceptionParameter (Catch (FormalParam _ _ _ _ vardeclId) _) = [identFromVarDeclId vardeclId]

methodParameterIdents :: MemberDecl p -> [Ident]
methodParameterIdents (MethodDecl _ _ _ _ _ params _ _ _) = formalParamIdent params
methodParameterIdents (ConstructorDecl _ _ _ _ params _ _) = formalParamIdent params
methodParameterIdents _ = []

formalParamIdent :: [FormalParam p] -> [Ident]
formalParamIdent = map (identFromVarDeclId . (\(FormalParam _ _ _ _ vardeclId) -> vardeclId))

localVarIdentsFromTryResources :: [TryResource p] -> [Ident]
localVarIdentsFromTryResources =
  mapMaybe
    ( \case
        (TryResourceVarDecl (ResourceDecl _ _ vardDeclId _)) -> Just (identFromVarDeclId vardDeclId)
        _ -> Nothing
    )

localVarsIdentsFromBasicForInit :: Maybe (ForInit p) -> [Ident]
localVarsIdentsFromBasicForInit (Just (ForLocalVars _ _ varDecls)) = map (identFromVarDeclId . (\(VarDecl _ varid _) -> varid)) varDecls
localVarsIdentsFromBasicForInit _ = []

-- boiler plate cases

instance AnalyzedTransformer TypeDecl where
  transformToAnalyzed scope (ClassTypeDecl classDecl) = ClassTypeDecl (transformToAnalyzed scope classDecl)
  transformToAnalyzed scope (InterfaceTypeDecl interfaceDecl) = InterfaceTypeDecl (transformToAnalyzed scope interfaceDecl)

instance AnalyzedTransformer InterfaceDecl where
  transformToAnalyzed scope (InterfaceDecl src kind modifiers idnt typeParams refTypesExtends refTypesPermits interfaceBody) =
    InterfaceDecl
      src
      kind
      (map (transformToAnalyzed scope) modifiers)
      idnt
      typeParams
      refTypesExtends
      refTypesPermits
      (transformToAnalyzed scope interfaceBody)

instance AnalyzedTransformer Modifier where
  transformToAnalyzed _ (Public src) = Public src
  transformToAnalyzed _ Private = Private
  transformToAnalyzed _ Protected = Protected
  transformToAnalyzed _ (Abstract src) = Abstract src
  transformToAnalyzed _ Final = Final
  transformToAnalyzed _ Static = Static
  transformToAnalyzed _ StrictFP = StrictFP
  transformToAnalyzed _ Transient = Transient
  transformToAnalyzed _ Volatile = Volatile
  transformToAnalyzed _ Native = Native
  transformToAnalyzed scope (Annotation anno) = Annotation (transformToAnalyzed scope anno)
  transformToAnalyzed _ Synchronized_ = Synchronized_
  transformToAnalyzed _ Sealed = Sealed

instance AnalyzedTransformer Annotation where
  transformToAnalyzed scope (NormalAnnotation srcspan annoName annkV) =
    NormalAnnotation
      srcspan
      annoName
      (map (second (transformToAnalyzed scope)) annkV)
  transformToAnalyzed scope (SingleElementAnnotation srcspan annoName annoValue) =
    SingleElementAnnotation srcspan annoName (transformToAnalyzed scope annoValue)
  transformToAnalyzed _ (MarkerAnnotation srcspan annoName) = MarkerAnnotation srcspan annoName

instance AnalyzedTransformer ClassBody where
  transformToAnalyzed scope (ClassBody decls) = ClassBody (map (transformToAnalyzed scope) decls)

instance AnalyzedTransformer EnumBody where
  transformToAnalyzed scope (EnumBody enumConstants decls) = EnumBody (map (transformToAnalyzed scope) enumConstants) (map (transformToAnalyzed scope) decls)

instance AnalyzedTransformer InterfaceBody where
  transformToAnalyzed scope (InterfaceBody memberDecls) = InterfaceBody (map (transformToAnalyzed scope) memberDecls)

instance AnalyzedTransformer ElementValue where
  transformToAnalyzed scope (EVVal varInit) = EVVal (transformToAnalyzed scope varInit)
  transformToAnalyzed scope (EVAnn anno) = EVAnn (transformToAnalyzed scope anno)

instance AnalyzedTransformer Decl where
  transformToAnalyzed scope (MemberDecl memberDecl) = MemberDecl (transformToAnalyzed scope memberDecl)
  transformToAnalyzed scope (InitDecl static block) = InitDecl static (transformToAnalyzed scope block)

instance AnalyzedTransformer EnumConstant where
  transformToAnalyzed scope (EnumConstant idnt arguments mbClassBody) = EnumConstant idnt (map (transformToAnalyzed scope) arguments) (fmap (transformToAnalyzed scope) mbClassBody)

instance AnalyzedTransformer VarInit where
  transformToAnalyzed scope (InitExp expr) = InitExp (transformToAnalyzed scope expr)
  transformToAnalyzed scope (InitArray arrayInit) = InitArray (transformToAnalyzed scope arrayInit)

instance AnalyzedTransformer BlockStmt where
  transformToAnalyzed scope (BlockStmt srcspan stmt) = BlockStmt srcspan (transformToAnalyzed scope stmt)
  transformToAnalyzed scope (LocalClass classDecl) = LocalClass (transformToAnalyzed scope classDecl)
  transformToAnalyzed scope (LocalVars srcspan modifiers type_ varDecls) =
    LocalVars
      srcspan
      (map (transformToAnalyzed scope) modifiers)
      type_
      (map (transformToAnalyzed scope) varDecls)

instance AnalyzedTransformer Exp where
  transformToAnalyzed _ (Lit literal) = Lit literal
  transformToAnalyzed _ (ClassLit mbType) = ClassLit mbType
  transformToAnalyzed _ This = This
  transformToAnalyzed _ (ThisClass name) = ThisClass name
  transformToAnalyzed scope (InstanceCreation typeArgs typeDeclSpec arguments mbClassBody) =
    InstanceCreation
      typeArgs
      typeDeclSpec
      (map (transformToAnalyzed scope) arguments)
      (fmap (transformToAnalyzed scope) mbClassBody)
  transformToAnalyzed scope (QualInstanceCreation expr typeArgs idnt arguments mbClassBody) =
    QualInstanceCreation
      (transformToAnalyzed scope expr)
      typeArgs
      idnt
      (map (transformToAnalyzed scope) arguments)
      (fmap (transformToAnalyzed scope) mbClassBody)
  transformToAnalyzed scope (ArrayCreate type_ exps int) = ArrayCreate type_ (map (transformToAnalyzed scope) exps) int
  transformToAnalyzed scope (ArrayCreateInit type_ int arrayInit) = ArrayCreateInit type_ int (transformToAnalyzed scope arrayInit)
  transformToAnalyzed scope (FieldAccess fieldAccess) = FieldAccess (transformToAnalyzed scope fieldAccess)
  transformToAnalyzed scope (MethodInv methodInv) = MethodInv (transformToAnalyzed scope methodInv)
  transformToAnalyzed scope (ArrayAccess arrayIndex) = ArrayAccess (transformToAnalyzed scope arrayIndex)
  transformToAnalyzed _ (ExpName name) = ExpName name
  transformToAnalyzed scope (PostIncrement srcspan expr) = PostIncrement srcspan (transformToAnalyzed scope expr)
  transformToAnalyzed scope (PostDecrement srcspan expr) = PostDecrement srcspan (transformToAnalyzed scope expr)
  transformToAnalyzed scope (PreIncrement srcspan expr) = PreIncrement srcspan (transformToAnalyzed scope expr)
  transformToAnalyzed scope (PreDecrement srcspan expr) = PreDecrement srcspan (transformToAnalyzed scope expr)
  transformToAnalyzed scope (PrePlus srcspan expr) = PrePlus srcspan (transformToAnalyzed scope expr)
  transformToAnalyzed scope (PreMinus srcspan expr) = PreMinus srcspan (transformToAnalyzed scope expr)
  transformToAnalyzed scope (PreBitCompl srcspan expr) = PreBitCompl srcspan (transformToAnalyzed scope expr)
  transformToAnalyzed scope (PreNot srcspan expr) = PreNot srcspan (transformToAnalyzed scope expr)
  transformToAnalyzed scope (Cast type_ expr) = Cast type_ (transformToAnalyzed scope expr)
  transformToAnalyzed scope (BinOp exp1 op exp2) = BinOp (transformToAnalyzed scope exp1) op (transformToAnalyzed scope exp2)
  transformToAnalyzed scope (InstanceOf expr refType mbName) = InstanceOf (transformToAnalyzed scope expr) refType mbName
  transformToAnalyzed scope (Cond srcspan exp1 exp2 exp3) = Cond srcspan (transformToAnalyzed scope exp1) (transformToAnalyzed scope exp2) (transformToAnalyzed scope exp3)
  transformToAnalyzed scope (Assign srcspan lhs assignOp expr) = Assign srcspan (transformToAnalyzed scope lhs) assignOp (transformToAnalyzed scope expr)
  transformToAnalyzed scope (Lambda lambdaParams lambdaExp) = Lambda (transformToAnalyzed scope lambdaParams) (transformToAnalyzed scope lambdaExp)
  transformToAnalyzed _ (MethodRef name target) = MethodRef name target
  transformToAnalyzed scope (SwitchExp expr branches) = SwitchExp (transformToAnalyzed scope expr) (map (transformToAnalyzed scope) branches)

instance AnalyzedTransformer FormalParam where
  transformToAnalyzed scope (FormalParam srcspan modifiers type_ bool varDeclId) = FormalParam srcspan (map (transformToAnalyzed scope) modifiers) type_ bool varDeclId

instance AnalyzedTransformer VarDecl where
  transformToAnalyzed scope (VarDecl srcspan varDeclId mbVarInit) = VarDecl srcspan varDeclId (fmap (transformToAnalyzed scope) mbVarInit)

instance AnalyzedTransformer MethodBody where
  transformToAnalyzed scope (MethodBody mbBlock) = MethodBody (fmap (transformToAnalyzed scope) mbBlock)

instance AnalyzedTransformer ConstructorBody where
  transformToAnalyzed scope (ConstructorBody mbInv blockStmts) = ConstructorBody (fmap (transformToAnalyzed scope) mbInv) (map (transformToAnalyzed scope) blockStmts)

instance AnalyzedTransformer ArrayInit where
  transformToAnalyzed scope (ArrayInit varInits) = ArrayInit (map (transformToAnalyzed scope) varInits)

instance AnalyzedTransformer FieldAccess where
  transformToAnalyzed scope (PrimaryFieldAccess expr idnt) = PrimaryFieldAccess (transformToAnalyzed scope expr) idnt
  transformToAnalyzed _ (SuperFieldAccess idnt) = SuperFieldAccess idnt
  transformToAnalyzed _ (ClassFieldAccess name idnt) = ClassFieldAccess name idnt

instance AnalyzedTransformer ArrayIndex where
  transformToAnalyzed scope (ArrayIndex expr exps) = ArrayIndex (transformToAnalyzed scope expr) (map (transformToAnalyzed scope) exps)

instance AnalyzedTransformer Lhs where
  transformToAnalyzed _ (NameLhs name) = NameLhs name
  transformToAnalyzed scope (FieldLhs fieldAccess) = FieldLhs (transformToAnalyzed scope fieldAccess)
  transformToAnalyzed scope (ArrayLhs arrayIndex) = ArrayLhs (transformToAnalyzed scope arrayIndex)

instance AnalyzedTransformer LambdaParams where
  transformToAnalyzed _ (LambdaSingleParam idnt) = LambdaSingleParam idnt
  transformToAnalyzed scope (LambdaFormalParams params) = LambdaFormalParams (map (transformToAnalyzed scope) params)
  transformToAnalyzed _ (LambdaInferredParams idents) = LambdaInferredParams idents

instance AnalyzedTransformer LambdaExpression where
  transformToAnalyzed scope (LambdaExpression expr) = LambdaExpression (transformToAnalyzed scope expr)
  transformToAnalyzed scope (LambdaBlock block) = LambdaBlock (transformToAnalyzed scope block)

instance AnalyzedTransformer ForInit where
  transformToAnalyzed scope (ForLocalVars modifiers type_ varDecls) = ForLocalVars (map (transformToAnalyzed scope) modifiers) type_ (map (transformToAnalyzed scope) varDecls)
  transformToAnalyzed scope (ForInitExps exps) = ForInitExps (map (transformToAnalyzed scope) exps)

instance AnalyzedTransformer SwitchExpBranch where
  transformToAnalyzed scope (SwitchExpBranch label body) = SwitchExpBranch (transformToAnalyzed scope label) (transformToAnalyzed scope body)

instance AnalyzedTransformer SwitchLabel where
  transformToAnalyzed scope (SwitchCase exps) = SwitchCase (map (transformToAnalyzed scope) exps)
  transformToAnalyzed _ Default = Default

instance AnalyzedTransformer SwitchExpBranchBody where
  transformToAnalyzed scope (SwitchExpBranchExp expr) = SwitchExpBranchExp (transformToAnalyzed scope expr)
  transformToAnalyzed scope (SwitchExpBranchBlock blockStmts) = SwitchExpBranchBlock (map (transformToAnalyzed scope) blockStmts)

instance AnalyzedTransformer ExplConstrInv where
  transformToAnalyzed scope (ThisInvoke refTypes args) = ThisInvoke refTypes (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (SuperInvoke refTypes args) = SuperInvoke refTypes (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (PrimarySuperInvoke expr refTypes args) = PrimarySuperInvoke (transformToAnalyzed scope expr) refTypes (map (transformToAnalyzed scope) args)

instance AnalyzedTransformer SwitchBlock where
  transformToAnalyzed scope (SwitchBlock srcspan switchLabel blockStmts) = SwitchBlock srcspan (transformToAnalyzed scope switchLabel) (map (transformToAnalyzed scope) blockStmts)

instance AnalyzedTransformer TryResource where
  transformToAnalyzed scope (TryResourceVarDecl resourceDecl) = TryResourceVarDecl (transformToAnalyzed scope resourceDecl)
  transformToAnalyzed _ (TryResourceVarAccess idnt) = TryResourceVarAccess idnt
  transformToAnalyzed scope (TryResourceQualAccess fieldAccess) = TryResourceQualAccess (transformToAnalyzed scope fieldAccess)

instance AnalyzedTransformer ResourceDecl where
  transformToAnalyzed scope (ResourceDecl modifiers type_ varDeclId varInit) = ResourceDecl (map (transformToAnalyzed scope) modifiers) type_ varDeclId (transformToAnalyzed scope varInit)