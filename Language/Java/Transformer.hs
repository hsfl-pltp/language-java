{-# LANGUAGE LambdaCase #-}

module Language.Java.Transformer (analyzeCompilationUnit) where

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe (mapMaybe)
import Language.Java.Syntax

analyzeCompilationUnit :: CompilationUnit Parsed -> CompilationUnit Analyzed
analyzeCompilationUnit = analyze (IdentCollection [] [] [])

class Transform a where
  analyze :: IdentCollection -> a Parsed -> a Analyzed

-- | data type used to collect the identifiers that are currently in scope
data IdentCollection = IdentCollection
  { fields :: [Ident],
    formalParams :: [Ident],
    localVars :: [Ident]
  }

addFields :: IdentCollection -> [Ident] -> IdentCollection
addFields identCollection idents = identCollection {fields = idents ++ fields identCollection}

addFormalParams :: IdentCollection -> [Ident] -> IdentCollection
addFormalParams identCollection idents = identCollection {formalParams = idents ++ formalParams identCollection}

addLocalVars :: IdentCollection -> [Ident] -> IdentCollection
addLocalVars identCollection idents = identCollection {localVars = idents ++ localVars identCollection}

isIdent :: Ident -> IdentCollection -> Bool
isIdent idnt (IdentCollection cfields fp vars) = any (eq IgnoreSourceSpan idnt) (cfields ++ fp ++ vars)

instance Transform ClassDecl where
  analyze scope (ClassDecl src modifiers idnt typeParams mbRefType refTypes classBody) =
    let newScope = addFields scope (fieldsFromClassBody classBody)
     in ClassDecl
          src
          (map (analyze scope) modifiers)
          idnt
          typeParams
          mbRefType
          refTypes
          (analyze newScope classBody)
  analyze scope (RecordDecl src modifiers idnt typeParams recordFieldDecls refTypes classBody) =
    let newScope = addFields scope (fieldsFromClassBody classBody)
     in RecordDecl
          src
          (map (analyze scope) modifiers)
          idnt
          typeParams
          recordFieldDecls
          refTypes
          (analyze newScope classBody)
  analyze scope (EnumDecl src modifiers idnt refTypes enumBody) =
    let newScope = addFields scope (fieldsFromEnumBody enumBody)
     in EnumDecl
          src
          (map (analyze scope) modifiers)
          idnt
          refTypes
          (analyze newScope enumBody)

instance Transform MemberDecl where
  analyze scope (FieldDecl srcspan modifiers type_ varDecls) =
    FieldDecl srcspan (map (analyze scope) modifiers) type_ (map (analyze scope) varDecls)
  analyze scope methodDecl@(MethodDecl srcspan modifiers typeParams mbType idnt fp exceptionTypes mbExp methodBody) =
    MethodDecl
      srcspan
      (map (analyze scope) modifiers)
      typeParams
      mbType
      idnt
      (map (analyze scope) fp)
      exceptionTypes
      (fmap (analyze scope) mbExp)
      (analyze (addFormalParams scope (methodParameters methodDecl)) methodBody)
  analyze scope constructorDecl@(ConstructorDecl srcspan modifiers typeParams idnt fp exceptionTypes constructorBody) =
    ConstructorDecl
      srcspan
      (map (analyze scope) modifiers)
      typeParams
      idnt
      (map (analyze scope) fp)
      exceptionTypes
      (analyze (addFormalParams scope (methodParameters constructorDecl)) constructorBody)
  analyze scope (MemberClassDecl classDecl) = MemberClassDecl (analyze scope classDecl)
  analyze scope (MemberInterfaceDecl interfaceDecl) = MemberInterfaceDecl (analyze scope interfaceDecl)

instance Transform Catch where
  analyze scope catch@(Catch formalParam block) =
    Catch
      (analyze scope formalParam)
      (analyze (addFormalParams scope (exceptionParameter catch)) block)

instance Transform Block where
  analyze scope (Block blockstmts) = Block (analyzeBlockStmts scope blockstmts)

instance Transform Stmt where
  analyze scope (StmtBlock block) = StmtBlock (analyze scope block)
  analyze scope (IfThen srcspan expr stmt) = IfThen srcspan (analyze scope expr) (analyze scope stmt)
  analyze scope (IfThenElse srcspan expr stmt1 stmt2) = IfThenElse srcspan (analyze scope expr) (analyze scope stmt1) (analyze scope stmt2)
  analyze scope (While srcspan expr stmt) = While srcspan (analyze scope expr) (analyze scope stmt)
  analyze scope (BasicFor srcspan mbForInit mbExp mbExps stmt) =
    let newScope = addLocalVars scope (localVarsInBasicForInit mbForInit)
     in BasicFor
          srcspan
          (fmap (analyze scope) mbForInit)
          (fmap (analyze newScope) mbExp)
          (fmap (map (analyze newScope)) mbExps)
          (analyze newScope stmt)
  analyze scope (EnhancedFor srcspan mods type_ idnt expr stmt) =
    EnhancedFor
      srcspan
      (map (analyze scope) mods)
      type_
      idnt
      (analyze scope expr)
      (analyze (addLocalVars scope [idnt]) stmt)
  analyze _ Empty = Empty
  analyze scope (ExpStmt srcspan expr) = ExpStmt srcspan (analyze scope expr)
  analyze scope (Assert expr mbExp) = Assert (analyze scope expr) (fmap (analyze scope) mbExp)
  analyze scope (Switch switchStyle expr switchBlocks) = Switch switchStyle (analyze scope expr) (map (analyze scope) switchBlocks)
  analyze scope (Do srcspan expr stmt) = Do srcspan (analyze scope expr) (analyze scope stmt)
  analyze _ (Break srcspan mbIdent) = Break srcspan mbIdent
  analyze _ (Continue mbIdent) = Continue mbIdent
  analyze scope (Return srcspan mbExp) = Return srcspan (fmap (analyze scope) mbExp)
  analyze scope (Synchronized expr block) = Synchronized (analyze scope expr) (analyze scope block)
  analyze scope (Throw expr) = Throw (analyze scope expr)
  analyze scope (Try srcspan tryResources block catches mbBlock) =
    Try
      srcspan
      (map (analyze scope) tryResources)
      (analyze (addLocalVars scope (localVarsInTryResources tryResources)) block)
      (map (analyze scope) catches)
      (fmap (analyze scope) mbBlock)
  analyze scope (Labeled idnt stmt) = Labeled idnt (analyze scope stmt)

-- classification of MethodInvocation name
instance Transform MethodInvocation where
  -- Todo : classify multipart name
  analyze scope (MethodCall Nothing idnt args) = MethodCall Nothing idnt (map (analyze scope) args)
  analyze scope (MethodCall (Just name@(Name _ [singleId])) idnt args) = MethodCall (Just (classifyName singleId scope name)) idnt (map (analyze scope) args)
  analyze scope (MethodCall _ idnt args) = MethodCall Nothing idnt (map (analyze scope) args)
  analyze scope (PrimaryMethodCall expr refTypes idnt args) = PrimaryMethodCall (analyze scope expr) refTypes idnt (map (analyze scope) args)
  analyze scope (SuperMethodCall refTypes idnt args) = SuperMethodCall refTypes idnt (map (analyze scope) args)
  analyze scope (ClassMethodCall name refTypes idnt args) = ClassMethodCall name refTypes idnt (map (analyze scope) args)
  analyze scope (TypeMethodCall name refTypes idnt args) = TypeMethodCall name refTypes idnt (map (analyze scope) args)

classifyName :: Ident -> IdentCollection -> Name -> ClassifiedName
classifyName idnt ic =
  if isIdent idnt ic then ExpressionName else TypeName

-- helper functions

identFromVarDeclId :: VarDeclId -> Ident
identFromVarDeclId (VarId idnt) = idnt
identFromVarDeclId (VarDeclArray _ varDeclId) = identFromVarDeclId varDeclId

-- class fields from ClassDecl

fieldsFromClassBody :: ClassBody p -> [Ident]
fieldsFromClassBody (ClassBody decls) = identsfromDecls decls

fieldsFromEnumBody :: EnumBody p -> [Ident]
fieldsFromEnumBody (EnumBody _ decls) = identsfromDecls decls

identsfromDecls :: [Decl p] -> [Ident]
identsfromDecls decls = do
  map
    (identFromVarDeclId . (\(VarDecl _ varId _) -> varId))
    ( concatMap
        ( \case
            (MemberDecl (FieldDecl _ _ _ vars)) -> vars
            _ -> []
        )
        decls
    )

-- | idnt of exception parameter from Catch
exceptionParameter :: Catch p -> [Ident]
exceptionParameter (Catch (FormalParam _ _ _ _ vardeclId) _) = [identFromVarDeclId vardeclId]

-- | idents of formal parameters of method and constructor declaration
methodParameters :: MemberDecl p -> [Ident]
methodParameters (MethodDecl _ _ _ _ _ params _ _ _) = identsFromParams params
methodParameters (ConstructorDecl _ _ _ _ params _ _) = identsFromParams params
methodParameters _ = []

identsFromParams :: [FormalParam p] -> [Ident]
identsFromParams = map (identFromVarDeclId . (\(FormalParam _ _ _ _ vardeclId) -> vardeclId))

-- | idents of local variables from basicFor, enhancedFor and tryWithResource
localVarsInTryResources :: [TryResource p] -> [Ident]
localVarsInTryResources = mapMaybe identFromTryResource
  where
    identFromTryResource (TryResourceVarDecl (ResourceDecl _ _ vardDeclId _)) = Just (identFromVarDeclId vardDeclId)
    identFromTryResource _ = Nothing

localVarsInBasicForInit :: Maybe (ForInit p) -> [Ident]
localVarsInBasicForInit (Just (ForLocalVars _ _ varDecls)) = map (identFromVarDeclId . (\(VarDecl _ varid _) -> varid)) varDecls
localVarsInBasicForInit _ = []

analyzeBlockStmts :: IdentCollection -> [BlockStmt Parsed] -> [BlockStmt Analyzed]
analyzeBlockStmts __ [] = []
analyzeBlockStmts scope (x@(LocalVars _ _ _ varDecls) : xs) =
  let newscope = addLocalVars scope (map (identFromVarDeclId . (\(VarDecl _ varId _) -> varId)) varDecls)
   in analyze scope x : analyzeBlockStmts newscope xs
analyzeBlockStmts scope (x : xs) = analyze scope x : analyzeBlockStmts scope xs

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
  analyze scope (InterfaceDecl src kind modifiers idnt typeParams refTypesExtends refTypesPermits interfaceBody) =
    InterfaceDecl
      src
      kind
      (map (analyze scope) modifiers)
      idnt
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
  analyze scope (NormalAnnotation srcspan annoName annkV) =
    NormalAnnotation
      srcspan
      annoName
      (map (second (analyze scope)) annkV)
  analyze scope (SingleElementAnnotation srcspan annoName annoValue) =
    SingleElementAnnotation srcspan annoName (analyze scope annoValue)
  analyze _ (MarkerAnnotation srcspan annoName) = MarkerAnnotation srcspan annoName

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
  analyze scope (EnumConstant idnt arguments mbClassBody) = EnumConstant idnt (map (analyze scope) arguments) (fmap (analyze scope) mbClassBody)

instance Transform VarInit where
  analyze scope (InitExp expr) = InitExp (analyze scope expr)
  analyze scope (InitArray arrayInit) = InitArray (analyze scope arrayInit)

instance Transform BlockStmt where
  analyze scope (BlockStmt srcspan stmt) = BlockStmt srcspan (analyze scope stmt)
  analyze scope (LocalClass classDecl) = LocalClass (analyze scope classDecl)
  analyze scope (LocalVars srcspan modifiers type_ varDecls) =
    LocalVars
      srcspan
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
  analyze scope (QualInstanceCreation expr typeArgs idnt arguments mbClassBody) =
    QualInstanceCreation
      (analyze scope expr)
      typeArgs
      idnt
      (map (analyze scope) arguments)
      (fmap (analyze scope) mbClassBody)
  analyze scope (ArrayCreate type_ exps int) = ArrayCreate type_ (map (analyze scope) exps) int
  analyze scope (ArrayCreateInit type_ int arrayInit) = ArrayCreateInit type_ int (analyze scope arrayInit)
  analyze scope (FieldAccess fieldAccess) = FieldAccess (analyze scope fieldAccess)
  analyze scope (MethodInv methodInv) = MethodInv (analyze scope methodInv)
  analyze scope (ArrayAccess arrayIndex) = ArrayAccess (analyze scope arrayIndex)
  analyze _ (ExpName name) = ExpName name
  analyze scope (PostIncrement srcspan expr) = PostIncrement srcspan (analyze scope expr)
  analyze scope (PostDecrement srcspan expr) = PostDecrement srcspan (analyze scope expr)
  analyze scope (PreIncrement srcspan expr) = PreIncrement srcspan (analyze scope expr)
  analyze scope (PreDecrement srcspan expr) = PreDecrement srcspan (analyze scope expr)
  analyze scope (PrePlus expr) = PrePlus (analyze scope expr)
  analyze scope (PreMinus expr) = PreMinus (analyze scope expr)
  analyze scope (PreBitCompl expr) = PreBitCompl (analyze scope expr)
  analyze scope (PreNot expr) = PreNot (analyze scope expr)
  analyze scope (Cast type_ expr) = Cast type_ (analyze scope expr)
  analyze scope (BinOp exp1 op exp2) = BinOp (analyze scope exp1) op (analyze scope exp2)
  analyze scope (InstanceOf expr refType mbName) = InstanceOf (analyze scope expr) refType mbName
  analyze scope (Cond srcspan exp1 exp2 exp3) = Cond srcspan (analyze scope exp1) (analyze scope exp2) (analyze scope exp3)
  analyze scope (Assign srcspan lhs assignOp expr) = Assign srcspan (analyze scope lhs) assignOp (analyze scope expr)
  analyze scope (Lambda lambdaParams lambdaExp) = Lambda (analyze scope lambdaParams) (analyze scope lambdaExp)
  analyze _ (MethodRef name target) = MethodRef name target
  analyze scope (SwitchExp expr branches) = SwitchExp (analyze scope expr) (map (analyze scope) branches)

instance Transform FormalParam where
  analyze scope (FormalParam srcspan modifiers type_ bool varDeclId) = FormalParam srcspan (map (analyze scope) modifiers) type_ bool varDeclId

instance Transform VarDecl where
  analyze scope (VarDecl srcspan varDeclId mbVarInit) = VarDecl srcspan varDeclId (fmap (analyze scope) mbVarInit)

instance Transform MethodBody where
  analyze scope (MethodBody mbBlock) = MethodBody (fmap (analyze scope) mbBlock)

instance Transform ConstructorBody where
  analyze scope (ConstructorBody mbInv blockStmts) = ConstructorBody (fmap (analyze scope) mbInv) (map (analyze scope) blockStmts)

instance Transform ArrayInit where
  analyze scope (ArrayInit varInits) = ArrayInit (map (analyze scope) varInits)

instance Transform FieldAccess where
  analyze scope (PrimaryFieldAccess expr idnt) = PrimaryFieldAccess (analyze scope expr) idnt
  analyze _ (SuperFieldAccess idnt) = SuperFieldAccess idnt
  analyze _ (ClassFieldAccess name idnt) = ClassFieldAccess name idnt

instance Transform ArrayIndex where
  analyze scope (ArrayIndex expr exps) = ArrayIndex (analyze scope expr) (map (analyze scope) exps)

instance Transform Lhs where
  analyze _ (NameLhs name) = NameLhs name
  analyze scope (FieldLhs fieldAccess) = FieldLhs (analyze scope fieldAccess)
  analyze scope (ArrayLhs arrayIndex) = ArrayLhs (analyze scope arrayIndex)

instance Transform LambdaParams where
  analyze _ (LambdaSingleParam idnt) = LambdaSingleParam idnt
  analyze scope (LambdaFormalParams params) = LambdaFormalParams (map (analyze scope) params)
  analyze _ (LambdaInferredParams idents) = LambdaInferredParams idents

instance Transform LambdaExpression where
  analyze scope (LambdaExpression expr) = LambdaExpression (analyze scope expr)
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
  analyze scope (SwitchExpBranchExp expr) = SwitchExpBranchExp (analyze scope expr)
  analyze scope (SwitchExpBranchBlock blockStmts) = SwitchExpBranchBlock (map (analyze scope) blockStmts)

instance Transform ExplConstrInv where
  analyze scope (ThisInvoke refTypes args) = ThisInvoke refTypes (map (analyze scope) args)
  analyze scope (SuperInvoke refTypes args) = SuperInvoke refTypes (map (analyze scope) args)
  analyze scope (PrimarySuperInvoke expr refTypes args) = PrimarySuperInvoke (analyze scope expr) refTypes (map (analyze scope) args)

instance Transform SwitchBlock where
  analyze scope (SwitchBlock srcspan switchLabel blockStmts) = SwitchBlock srcspan (analyze scope switchLabel) (map (analyze scope) blockStmts)

instance Transform TryResource where
  analyze scope (TryResourceVarDecl resourceDecl) = TryResourceVarDecl (analyze scope resourceDecl)
  analyze _ (TryResourceVarAccess idnt) = TryResourceVarAccess idnt
  analyze scope (TryResourceQualAccess fieldAccess) = TryResourceQualAccess (analyze scope fieldAccess)

instance Transform ResourceDecl where
  analyze scope (ResourceDecl modifiers type_ varDeclId varInit) = ResourceDecl (map (analyze scope) modifiers) type_ varDeclId (analyze scope varInit)