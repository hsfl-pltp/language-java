{-# LANGUAGE LambdaCase #-}

module Language.Java.Transformer (transformCompilationUnitToAnalyzed) where

import Data.Bifunctor (Bifunctor (second))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Language.Java.Syntax
import Language.Java.Syntax.ClassInfo as ClassInfo (ClassInfo (..), fromClassDecl, hasField, hasIdent)
import qualified Language.Java.Syntax.Decl as Decl
import qualified Language.Java.Syntax.FormalParam as FormalParam
import Language.Java.Syntax.IdentCollection as IdentCollection
  ( IdentCollection (..),
    addToClassInfos,
    addToFields,
    addToFormalParams,
    addToImportedClasses,
    addToLocalVars,
    empty,
    isExpressionIdent,
    isImportedClass,
  )
import qualified Language.Java.Syntax.TryResource as TryResource
import qualified Language.Java.Syntax.TypeDecl as TypeDecl
import qualified Language.Java.Syntax.VarDecl as VarDecl

-- | Entry point for transforming a compilationunit from parsed to analyzed and classifying all methodinvoker names
transformCompilationUnitToAnalyzed :: CompilationUnit Parsed -> CompilationUnit Analyzed
transformCompilationUnitToAnalyzed = transformToAnalyzed IdentCollection.empty

classifyName :: NonEmpty Ident -> IdentCollection -> Name -> ClassifiedName
classifyName (idnt :| rest) ic name
  | IdentCollection.isExpressionIdent idnt ic = ExpressionName name
  | IdentCollection.isImportedClass idnt ic = case rest of
      [] -> TypeName name
      _ -> Unknown name
  | otherwise = maybe (Unknown name) (classifyIdentFromTree rest name) (find (ClassInfo.hasIdent idnt) (icClassInfos ic))

classifyIdentFromTree :: [Ident] -> Name -> ClassInfo -> ClassifiedName
classifyIdentFromTree [] name _ = TypeName name
classifyIdentFromTree (idnt : rest) name classInfo
  | ClassInfo.hasField idnt classInfo = ExpressionName name
  | otherwise =
      maybe (Unknown name) (classifyIdentFromTree rest name) (find (ClassInfo.hasIdent idnt) (ciInnerClasses classInfo))

class AnalyzedTransformer a where
  transformToAnalyzed :: IdentCollection -> a Parsed -> a Analyzed

instance AnalyzedTransformer MethodInvocation where
  transformToAnalyzed scope (MethodCall Nothing idnt args) = MethodCall Nothing idnt (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (MethodCall (Just name@(Name _ idents)) idnt args) = MethodCall (Just (classifyName idents scope name)) idnt (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (PrimaryMethodCall expr refTypes idnt args) = PrimaryMethodCall (transformToAnalyzed scope expr) refTypes idnt (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (SuperMethodCall refTypes idnt args) = SuperMethodCall refTypes idnt (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (ClassMethodCall name refTypes idnt args) = ClassMethodCall name refTypes idnt (map (transformToAnalyzed scope) args)
  transformToAnalyzed scope (TypeMethodCall name refTypes idnt args) = TypeMethodCall name refTypes idnt (map (transformToAnalyzed scope) args)

instance AnalyzedTransformer CompilationUnit where
  transformToAnalyzed scope (CompilationUnit mbPackageDecl importDecls typeDecls) =
    let newscope =
          ( IdentCollection.addToClassInfos
              (map ClassInfo.fromClassDecl (mapMaybe TypeDecl.classDecl typeDecls))
              . IdentCollection.addToImportedClasses
                ( mapMaybe
                    ( \case
                        (ImportDecl _ False (Name _ idents) False) -> Just (NonEmpty.last idents)
                        _ -> Nothing
                    )
                    importDecls
                )
          )
            scope
     in CompilationUnit
          mbPackageDecl
          importDecls
          (map (transformToAnalyzed newscope) typeDecls)

instance AnalyzedTransformer ClassDecl where
  transformToAnalyzed scope (ClassDecl src modifiers idnt typeParams mbRefType refTypes classBody@(ClassBody decls)) =
    let newScope =
          ( IdentCollection.addToClassInfos (map ClassInfo.fromClassDecl (mapMaybe Decl.classDecl decls))
              . IdentCollection.addToFields (concat (mapMaybe Decl.fieldIdents decls))
          )
            scope
     in ClassDecl
          src
          (map (transformToAnalyzed scope) modifiers)
          idnt
          typeParams
          mbRefType
          refTypes
          (transformToAnalyzed newScope classBody)
  transformToAnalyzed scope (RecordDecl src modifiers idnt typeParams recordFieldDecls refTypes classBody@(ClassBody decls)) =
    let newScope =
          ( IdentCollection.addToClassInfos (map ClassInfo.fromClassDecl (mapMaybe Decl.classDecl decls))
              . IdentCollection.addToFields (concat (mapMaybe Decl.fieldIdents decls))
          )
            scope
     in RecordDecl
          src
          (map (transformToAnalyzed scope) modifiers)
          idnt
          typeParams
          recordFieldDecls
          refTypes
          (transformToAnalyzed newScope classBody)
  transformToAnalyzed scope (EnumDecl src modifiers idnt refTypes enumBody@(EnumBody cons decls)) =
    let newScope =
          ( IdentCollection.addToClassInfos (map ClassInfo.fromClassDecl (mapMaybe Decl.classDecl decls))
              . IdentCollection.addToFields (concat (mapMaybe Decl.fieldIdents decls) ++ map (\(EnumConstant idnt' _ _) -> idnt') cons)
          )
            scope
     in EnumDecl
          src
          (map (transformToAnalyzed scope) modifiers)
          idnt
          refTypes
          (transformToAnalyzed newScope enumBody)

instance AnalyzedTransformer MemberDecl where
  transformToAnalyzed scope (FieldDecl srcspan modifiers type_ varDecls) =
    FieldDecl srcspan (map (transformToAnalyzed scope) modifiers) type_ (NonEmpty.map (transformToAnalyzed scope) varDecls)
  transformToAnalyzed scope (MethodDecl srcspan modifiers typeParams mbType idnt fp exceptionTypes mbExp methodBody) =
    MethodDecl
      srcspan
      (map (transformToAnalyzed scope) modifiers)
      typeParams
      mbType
      idnt
      (map (transformToAnalyzed scope) fp)
      exceptionTypes
      (fmap (transformToAnalyzed scope) mbExp)
      (transformToAnalyzed (IdentCollection.addToFormalParams (map FormalParam.ident fp) scope) methodBody)
  transformToAnalyzed scope (ConstructorDecl srcspan modifiers typeParams idnt fp exceptionTypes constructorBody) =
    ConstructorDecl
      srcspan
      (map (transformToAnalyzed scope) modifiers)
      typeParams
      idnt
      (map (transformToAnalyzed scope) fp)
      exceptionTypes
      (transformToAnalyzed (IdentCollection.addToFormalParams (map FormalParam.ident fp) scope) constructorBody)
  transformToAnalyzed scope (MemberClassDecl classDecl) = MemberClassDecl (transformToAnalyzed scope classDecl)
  transformToAnalyzed scope (MemberInterfaceDecl interfaceDecl) = MemberInterfaceDecl (transformToAnalyzed scope interfaceDecl)

instance AnalyzedTransformer Catch where
  transformToAnalyzed scope (Catch formalParam block) =
    Catch
      (transformToAnalyzed scope formalParam)
      (transformToAnalyzed (IdentCollection.addToFormalParams [FormalParam.ident formalParam] scope) block)

instance AnalyzedTransformer Block where
  transformToAnalyzed scope (Block blockstmts) = Block (transformBlockStmtsToAnalyzed scope blockstmts)

transformBlockStmtsToAnalyzed :: IdentCollection -> [BlockStmt Parsed] -> [BlockStmt Analyzed]
transformBlockStmtsToAnalyzed _ [] = []
transformBlockStmtsToAnalyzed scope (blckStmt@(LocalVars _ _ _ varDecls) : rest) =
  let newscope = IdentCollection.addToLocalVars (map VarDecl.ident (NonEmpty.toList varDecls)) scope
   in transformToAnalyzed newscope blckStmt : transformBlockStmtsToAnalyzed newscope rest
transformBlockStmtsToAnalyzed scope (blckStmt@(LocalClass classDecl) : rest) =
  let newscope = IdentCollection.addToClassInfos [ClassInfo.fromClassDecl classDecl] scope
   in transformToAnalyzed newscope blckStmt : transformBlockStmtsToAnalyzed newscope rest
transformBlockStmtsToAnalyzed scope (blckStmt : rest) = transformToAnalyzed scope blckStmt : transformBlockStmtsToAnalyzed scope rest

instance AnalyzedTransformer Stmt where
  transformToAnalyzed scope (StmtBlock block) = StmtBlock (transformToAnalyzed scope block)
  transformToAnalyzed scope (IfThen srcspan expr stmt) = IfThen srcspan (transformToAnalyzed scope expr) (transformToAnalyzed scope stmt)
  transformToAnalyzed scope (IfThenElse srcspan expr stmt1 stmt2) = IfThenElse srcspan (transformToAnalyzed scope expr) (transformToAnalyzed scope stmt1) (transformToAnalyzed scope stmt2)
  transformToAnalyzed scope (While srcspan expr stmt) = While srcspan (transformToAnalyzed scope expr) (transformToAnalyzed scope stmt)
  transformToAnalyzed scope (BasicFor srcspan mbForInit mbExp exps stmt) =
    let newScope = case mbForInit of
          Just (ForLocalVars _ _ varDecls) -> IdentCollection.addToLocalVars (map VarDecl.ident (NonEmpty.toList varDecls)) scope
          _ -> scope
     in BasicFor
          srcspan
          (fmap (transformToAnalyzed scope) mbForInit)
          (fmap (transformToAnalyzed newScope) mbExp)
          (map (transformToAnalyzed newScope) exps)
          (transformToAnalyzed newScope stmt)
  transformToAnalyzed scope (EnhancedFor srcspan mods type_ idnt expr stmt) =
    EnhancedFor
      srcspan
      (map (transformToAnalyzed scope) mods)
      type_
      idnt
      (transformToAnalyzed scope expr)
      (transformToAnalyzed (IdentCollection.addToLocalVars [idnt] scope) stmt)
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
      (transformToAnalyzed (IdentCollection.addToLocalVars (mapMaybe TryResource.resourceDeclIdent tryResources) scope) block)
      (map (transformToAnalyzed scope) catches)
      (fmap (transformToAnalyzed scope) mbBlock)
  transformToAnalyzed scope (Labeled idnt stmt) = Labeled idnt (transformToAnalyzed scope stmt)

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
      (NonEmpty.map (second (transformToAnalyzed scope)) annkV)
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
  transformToAnalyzed scope (BlockStmt stmt) = BlockStmt (transformToAnalyzed scope stmt)
  transformToAnalyzed scope (LocalClass classDecl) = LocalClass (transformToAnalyzed scope classDecl)
  transformToAnalyzed scope (LocalVars srcspan modifiers type_ varDecls) =
    LocalVars
      srcspan
      (map (transformToAnalyzed scope) modifiers)
      type_
      (NonEmpty.map (transformToAnalyzed scope) varDecls)

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
  transformToAnalyzed scope (ArrayCreate type_ exps int) = ArrayCreate type_ (NonEmpty.map (transformToAnalyzed scope) exps) int
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
  transformToAnalyzed scope (SwitchExp expr branches) = SwitchExp (transformToAnalyzed scope expr) (NonEmpty.map (transformToAnalyzed scope) branches)

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
  transformToAnalyzed scope (ArrayIndex expr exps) = ArrayIndex (transformToAnalyzed scope expr) (NonEmpty.map (transformToAnalyzed scope) exps)

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
  transformToAnalyzed scope (ForLocalVars modifiers type_ varDecls) = ForLocalVars (map (transformToAnalyzed scope) modifiers) type_ (NonEmpty.map (transformToAnalyzed scope) varDecls)
  transformToAnalyzed scope (ForInitExps exps) = ForInitExps (NonEmpty.map (transformToAnalyzed scope) exps)

instance AnalyzedTransformer SwitchExpBranch where
  transformToAnalyzed scope (SwitchExpBranch label body) = SwitchExpBranch (transformToAnalyzed scope label) (transformToAnalyzed scope body)

instance AnalyzedTransformer SwitchLabel where
  transformToAnalyzed scope (SwitchCase exps) = SwitchCase (NonEmpty.map (transformToAnalyzed scope) exps)
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
