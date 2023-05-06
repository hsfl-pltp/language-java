{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Java.Syntax
  ( CompilationUnit (..),
    PackageDecl (..),
    ImportDecl (..),
    TypeDecl (..),
    ClassDecl (..),
    ClassBody (..),
    EnumBody (..),
    EnumConstant (..),
    InterfaceDecl (..),
    InterfaceBody (..),
    InterfaceKind (..),
    Decl (..),
    MemberDecl (..),
    RecordFieldDecl (..),
    VarDecl (..),
    VarDeclId (..),
    VarInit (..),
    FormalParam (..),
    MethodBody (..),
    ConstructorBody (..),
    ExplConstrInv (..),
    Modifier (..),
    Annotation (..),
    desugarAnnotation,
    desugarAnnotation',
    ElementValue (..),
    Block (..),
    BlockStmt (..),
    Stmt (..),
    Catch (..),
    TryResource (..),
    ResourceDecl (..),
    SwitchBlock (..),
    SwitchLabel (..),
    SwitchExpBranch (..),
    SwitchExpBranchBody (..),
    SwitchStyle (..),
    ForInit (..),
    ExceptionType,
    Argument,
    Exp (..),
    Lhs (..),
    ArrayIndex (..),
    FieldAccess (..),
    LambdaParams (..),
    LambdaExpression (..),
    ArrayInit (..),
    MethodInvocation (..),
    MethodRefTarget (..),
    module Language.Java.Syntax.Exp,
    module Language.Java.Syntax.Types,
  )
where

import Data.Data
import GHC.Generics (Generic)
import Language.Java.SourceSpan
import Language.Java.Syntax.Equality (Equality (..))
import Language.Java.Syntax.Exp
import Language.Java.Syntax.Types

-----------------------------------------------------------------------
-- Packages

-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit = CompilationUnit (Maybe PackageDecl) [ImportDecl] [TypeDecl]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality CompilationUnit where
  eq opt (CompilationUnit mpd1 ids1 tds1) (CompilationUnit mpd2 ids2 tds2) =
    eq opt mpd1 mpd2 && eq opt ids1 ids2 && eq opt tds1 tds2

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
newtype PackageDecl = PackageDecl Name
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality PackageDecl where
  eq opt (PackageDecl n1) (PackageDecl n2) =
    eq opt n1 n2

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl
  = ImportDecl SourceSpan Bool {- static? -} Name Bool {- .*? -}
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ImportDecl where
  eq opt (ImportDecl s1 b11 n1 b12) (ImportDecl s2 b21 n2 b22) =
    eq opt s1 s2 && b11 == b21 && eq opt n1 n2 && b12 == b22

-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl
  = ClassTypeDecl ClassDecl
  | InterfaceTypeDecl InterfaceDecl
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality TypeDecl where
  eq opt (ClassTypeDecl cd1) (ClassTypeDecl cd2) =
    eq opt cd1 cd2
  eq opt (InterfaceTypeDecl id1) (InterfaceTypeDecl id2) =
    eq opt id1 id2
  eq _ _ _ = False

-- | A class declaration specifies a new named reference type.
data ClassDecl
  = ClassDecl SourceSpan [Modifier] Ident [TypeParam] (Maybe RefType) [RefType] ClassBody
  | RecordDecl SourceSpan [Modifier] Ident [TypeParam] [RecordFieldDecl] [RefType] ClassBody
  | EnumDecl SourceSpan [Modifier] Ident [RefType] EnumBody
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ClassDecl where
  eq opt (ClassDecl s1 ms1 i1 tps1 mrt1 rts1 cb1) (ClassDecl s2 ms2 i2 tps2 mrt2 rts2 cb2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt i1 i2 && eq opt tps1 tps2 && eq opt mrt1 mrt2 && eq opt rts1 rts2 && eq opt cb1 cb2
  eq opt (RecordDecl s1 ms1 i1 tps1 rfds1 rts1 cb1) (RecordDecl s2 ms2 i2 tps2 rfds2 rts2 cb2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt i1 i2 && eq opt tps1 tps2 && eq opt rfds1 rfds2 && eq opt rts1 rts2 && eq opt cb1 cb2
  eq opt (EnumDecl s1 ms1 i1 rts1 eb1) (EnumDecl s2 ms2 i2 rts2 eb2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt i1 i2 && eq opt rts1 rts2 && eq opt eb1 eb2
  eq _ _ _ = False

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
newtype ClassBody = ClassBody [Decl]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ClassBody where
  eq opt (ClassBody ds1) (ClassBody ds2) =
    eq opt ds1 ds2

-- | The body of an enum type may contain enum constants.
data EnumBody = EnumBody [EnumConstant] [Decl]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality EnumBody where
  eq opt (EnumBody ecs1 ds1) (EnumBody ecs2 ds2) =
    eq opt ecs1 ecs2 && eq opt ds1 ds2

-- | An enum constant defines an instance of the enum type.
data EnumConstant = EnumConstant Ident [Argument] (Maybe ClassBody)
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality EnumConstant where
  eq opt (EnumConstant i1 as1 mcb1) (EnumConstant i2 as2 mcb2) =
    eq opt i1 i2 && eq opt as1 as2 && eq opt mcb1 mcb2

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl
  = InterfaceDecl SourceSpan InterfaceKind [Modifier] Ident [TypeParam] [RefType {- extends -}] [RefType {- permits -}] InterfaceBody
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality InterfaceDecl where
  eq opt (InterfaceDecl s1 ik1 ms1 i1 tps1 rts11 rts12 ib1) (InterfaceDecl s2 ik2 ms2 i2 tps2 rts21 rts22 ib2) =
    eq opt s1 s2 && eq opt ik1 ik2 && eq opt ms1 ms2 && eq opt i1 i2 && eq opt tps1 tps2 && eq opt rts11 rts21 && eq opt rts12 rts22 && eq opt ib1 ib2

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality InterfaceKind where
  eq _ InterfaceNormal InterfaceNormal = True
  eq _ InterfaceAnnotation InterfaceAnnotation = True
  eq _ _ _ = False

-- | The body of an interface may declare members of the interface.
newtype InterfaceBody
  = InterfaceBody [MemberDecl]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality InterfaceBody where
  eq opt (InterfaceBody mds1) (InterfaceBody mds2) =
    eq opt mds1 mds2

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl
  = MemberDecl MemberDecl
  | InitDecl Bool Block
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Decl where
  eq opt (MemberDecl md1) (MemberDecl md2) =
    eq opt md1 md2
  eq opt (InitDecl b1 bl1) (InitDecl b2 bl2) =
    b1 == b2 && eq opt bl1 bl2
  eq _ _ _ = False

-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl
  = -- | The variables of a class type are introduced by field declarations.
    FieldDecl SourceSpan [Modifier] Type [VarDecl]
  | -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    MethodDecl SourceSpan [Modifier] [TypeParam] (Maybe Type) Ident [FormalParam] [ExceptionType] (Maybe Exp) MethodBody
  | -- | A constructor is used in the creation of an object that is an instance of a class.
    ConstructorDecl SourceSpan [Modifier] [TypeParam] Ident [FormalParam] [ExceptionType] ConstructorBody
  | -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    MemberClassDecl ClassDecl
  | -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    MemberInterfaceDecl InterfaceDecl
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality MemberDecl where
  eq opt (FieldDecl s1 ms1 t1 vds1) (FieldDecl s2 ms2 t2 vds2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt t1 t2 && eq opt vds1 vds2
  eq opt (MethodDecl s1 ms1 tps1 mt1 i1 fps1 ets1 me1 mb1) (MethodDecl s2 ms2 tps2 mt2 i2 fps2 ets2 me2 mb2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt tps1 tps2 && eq opt mt1 mt2 && eq opt i1 i2 && eq opt fps1 fps2 && eq opt ets1 ets2 && eq opt me1 me2 && eq opt mb1 mb2
  eq opt (ConstructorDecl s1 ms1 tps1 i1 fps1 ets1 cb1) (ConstructorDecl s2 ms2 tps2 i2 fps2 ets2 cb2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt tps1 tps2 && eq opt i1 i2 && eq opt fps1 fps2 && eq opt ets1 ets2 && eq opt cb1 cb2
  eq opt (MemberClassDecl cd1) (MemberClassDecl cd2) =
    eq opt cd1 cd2
  eq opt (MemberInterfaceDecl id1) (MemberInterfaceDecl id2) =
    eq opt id1 id2
  eq _ _ _ = False

-- | A field declaration of a record
data RecordFieldDecl
  = RecordFieldDecl Type Ident
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality RecordFieldDecl where
  eq opt (RecordFieldDecl t1 i1) (RecordFieldDecl t2 i2) =
    eq opt t1 t2 && eq opt i1 i2

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl
  = VarDecl SourceSpan VarDeclId (Maybe VarInit)
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality VarDecl where
  eq opt (VarDecl s1 vdi1 mvi1) (VarDecl s2 vdi2 mvi2) =
    eq opt s1 s2 && eq opt vdi1 vdi2 && eq opt mvi1 mvi2

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId
  = VarId Ident
  | -- | Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
    VarDeclArray SourceSpan VarDeclId
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality VarDeclId where
  eq opt (VarId i1) (VarId i2) =
    eq opt i1 i2
  eq opt (VarDeclArray s1 vdi1) (VarDeclArray s2 vdi2) =
    eq opt s1 s2 && eq opt vdi1 vdi2
  eq _ _ _ = False

-- | Explicit initializer for a variable declaration.
data VarInit
  = InitExp Exp
  | InitArray ArrayInit
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality VarInit where
  eq opt (InitExp e1) (InitExp e2) =
    eq opt e1 e2
  eq opt (InitArray ai1) (InitArray ai2) =
    eq opt ai1 ai2
  eq _ _ _ = False

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam = FormalParam SourceSpan [Modifier] Type Bool VarDeclId
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality FormalParam where
  eq opt (FormalParam s1 ms1 t1 b1 vdi1) (FormalParam s2 ms2 t2 b2 vdi2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt t1 t2 && b1 == b2 && eq opt vdi1 vdi2

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
newtype MethodBody = MethodBody (Maybe Block)
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality MethodBody where
  eq opt (MethodBody mb1) (MethodBody mb2) =
    eq opt mb1 mb2

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody = ConstructorBody (Maybe ExplConstrInv) [BlockStmt]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ConstructorBody where
  eq opt (ConstructorBody meci1 bss1) (ConstructorBody meci2 bss2) =
    eq opt meci1 meci2 && eq opt bss1 bss2

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv
  = ThisInvoke [RefType] [Argument]
  | SuperInvoke [RefType] [Argument]
  | PrimarySuperInvoke Exp [RefType] [Argument]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ExplConstrInv where
  eq opt (ThisInvoke rts1 as1) (ThisInvoke rts2 as2) =
    eq opt rts1 rts2 && eq opt as1 as2
  eq opt (SuperInvoke rts1 as1) (SuperInvoke rts2 as2) =
    eq opt rts1 rts2 && eq opt as1 as2
  eq opt (PrimarySuperInvoke e1 rts1 as1) (PrimarySuperInvoke e2 rts2 as2) =
    eq opt e1 e2 && eq opt rts1 rts2 && eq opt as1 as2
  eq _ _ _ = False

-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier
  = Public SourceSpan
  | Private
  | Protected
  | Abstract SourceSpan
  | Final
  | Static
  | StrictFP
  | Transient
  | Volatile
  | Native
  | Annotation Annotation
  | Synchronized_
  | Sealed
  deriving (Read, Typeable, Generic, Data)

instance Equality Modifier where
  eq opt (Public s1) (Public s2) =
    eq opt s1 s2
  eq _ Private Private = True
  eq _ Protected Protected = True
  eq opt (Abstract s1) (Abstract s2) =
    eq opt s1 s2
  eq _ Final Final = True
  eq _ Static Static = True
  eq _ StrictFP StrictFP = True
  eq _ Transient Transient = True
  eq _ Volatile Volatile = True
  eq _ Native Native = True
  eq opt (Annotation a1) (Annotation a2) =
    eq opt a1 a2
  eq _ Synchronized_ Synchronized_ = True
  eq _ Sealed Sealed = True
  eq _ _ _ = False

instance Show Modifier where
  show (Public _) = "public"
  show Private = "private"
  show Protected = "protected"
  show (Abstract _) = "abstract"
  show Final = "final"
  show Static = "static"
  show StrictFP = "strictfp"
  show Transient = "transient"
  show Volatile = "volatile"
  show Native = "native"
  show (Annotation a) = show a
  show Synchronized_ = "synchronized"
  show Sealed = "sealed"

-- | Annotations have three different forms: no-parameter, single-parameter or key-value pairs
data Annotation
  = NormalAnnotation
      { span :: SourceSpan,
        annName :: Name, -- Not type because not type generics not allowed
        annKV :: [(Ident, ElementValue)]
      }
  | SingleElementAnnotation
      { span :: SourceSpan,
        annName :: Name,
        annValue :: ElementValue
      }
  | MarkerAnnotation
      { span :: SourceSpan,
        annName :: Name
      }
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Annotation where
  eq opt (NormalAnnotation s1 n1 ievs1) (NormalAnnotation s2 n2 ievs2) =
    eq opt s1 s2 && eq opt n1 n2 && eq opt ievs1 ievs2
  eq opt (SingleElementAnnotation s1 n1 ev1) (SingleElementAnnotation s2 n2 ev2) =
    eq opt s1 s2 && eq opt n1 n2 && eq opt ev1 ev2
  eq opt (MarkerAnnotation s1 n1) (MarkerAnnotation s2 n2) =
    eq opt s1 s2 && eq opt n1 n2
  eq _ _ _ = False

desugarAnnotation (MarkerAnnotation span n) = (span, n, [])
-- TODO: check span for ident
desugarAnnotation (SingleElementAnnotation span n e) = (span, n, [(Ident dummySourceSpan "value", e)])
desugarAnnotation (NormalAnnotation span n kv) = (span, n, kv)

desugarAnnotation' (MarkerAnnotation span n) = NormalAnnotation span n []
-- TODO: check span for ident
desugarAnnotation' (SingleElementAnnotation span n e) = NormalAnnotation span n [(Ident dummySourceSpan "value", e)]
desugarAnnotation' normal = normal

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue
  = EVVal VarInit
  | EVAnn Annotation
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ElementValue where
  eq opt (EVVal vi1) (EVVal vi2) =
    eq opt vi1 vi2
  eq opt (EVAnn a1) (EVAnn a2) =
    eq opt a1 a2
  eq _ _ _ = False

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
newtype Block = Block [BlockStmt]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Block where
  eq opt (Block bss1) (Block bss2) =
    eq opt bss1 bss2

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt
  = BlockStmt SourceSpan Stmt
  | LocalClass ClassDecl
  | LocalVars SourceSpan [Modifier] Type [VarDecl]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality BlockStmt where
  eq opt (BlockStmt s1 stmt1) (BlockStmt s2 stmt2) =
    eq opt s1 s2 && eq opt stmt1 stmt2
  eq opt (LocalClass cd1) (LocalClass cd2) =
    eq opt cd1 cd2
  eq opt (LocalVars s1 ms1 t1 vds1) (LocalVars s2 ms2 t2 vds2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt t1 t2 && eq opt vds1 vds2
  eq _ _ _ = False

-- | A Java statement.
data Stmt
  = -- | A statement can be a nested block.
    StmtBlock Block
  | -- | The @if-then@ statement allows conditional execution of a statement.
    IfThen SourceSpan Exp Stmt
  | -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    IfThenElse SourceSpan Exp Stmt Stmt
  | -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    While SourceSpan Exp Stmt
  | -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    BasicFor SourceSpan (Maybe ForInit) (Maybe Exp) (Maybe [Exp]) Stmt
  | -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    EnhancedFor SourceSpan [Modifier] Type Ident Exp Stmt
  | -- | An empty statement does nothing.
    Empty
  | -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    ExpStmt SourceSpan Exp
  | -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    Assert Exp (Maybe Exp)
  | -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    Switch SwitchStyle Exp [SwitchBlock]
  | -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    Do SourceSpan Stmt Exp
  | -- | A @break@ statement transfers control out of an enclosing statement.
    Break SourceSpan (Maybe Ident)
  | -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    Continue (Maybe Ident)
  | -- A @return@ statement returns control to the invoker of a method or constructor.
    Return SourceSpan (Maybe Exp)
  | -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    Synchronized Exp Block
  | -- | A @throw@ statement causes an exception to be thrown.
    Throw Exp
  | -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    Try SourceSpan [TryResource] Block [Catch] (Maybe {- finally -} Block)
  | -- | Statements may have label prefixes.
    Labeled Ident Stmt
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Stmt where
  eq opt (StmtBlock b1) (StmtBlock b2) =
    eq opt b1 b2
  eq opt (IfThen s1 e1 stmt1) (IfThen s2 e2 stmt2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt stmt1 stmt2
  eq opt (IfThenElse s1 e1 stmt11 stmt12) (IfThenElse s2 e2 stmt21 stmt22) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt stmt11 stmt21 && eq opt stmt12 stmt22
  eq opt (While s1 e1 stmt1) (While s2 e2 stmt2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt stmt1 stmt2
  eq opt (BasicFor s1 mfi1 me1 mes1 stmt1) (BasicFor s2 mfi2 me2 mes2 stmt2) =
    eq opt s1 s2 && eq opt mfi1 mfi2 && eq opt me1 me2 && eq opt mes1 mes2 && eq opt stmt1 stmt2
  eq opt (EnhancedFor s1 ms1 t1 i1 e1 stmt1) (EnhancedFor s2 ms2 t2 i2 e2 stmt2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt t1 t2 && eq opt i1 i2 && eq opt e1 e2 && eq opt stmt1 stmt2
  eq _ Empty Empty = True
  eq opt (ExpStmt s1 e1) (ExpStmt s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (Assert e1 me1) (Assert e2 me2) =
    eq opt e1 e2 && eq opt me1 me2
  eq opt (Switch ss1 e1 sbs1) (Switch ss2 e2 sbs2) =
    eq opt ss1 ss2 && eq opt e1 e2 && eq opt sbs1 sbs2
  eq opt (Do s1 stmt1 e1) (Do s2 stmt2 e2) =
    eq opt s1 s2 && eq opt stmt1 stmt2 && eq opt e1 e2
  eq opt (Break s1 mi1) (Break s2 mi2) =
    eq opt s1 s2 && eq opt mi1 mi2
  eq opt (Continue mi1) (Continue mi2) =
    eq opt mi1 mi2
  eq opt (Return s1 me1) (Return s2 me2) =
    eq opt s1 s2 && eq opt me1 me2
  eq opt (Synchronized e1 b1) (Synchronized e2 b2) =
    eq opt e1 e2 && eq opt b1 b2
  eq opt (Throw e1) (Throw e2) =
    eq opt e1 e2
  eq opt (Try s1 trs1 b1 cs1 mb1) (Try s2 trs2 b2 cs2 mb2) =
    eq opt s1 s2 && eq opt trs1 trs2 && eq opt b1 b2 && eq opt cs1 cs2 && eq opt mb1 mb2
  eq opt (Labeled i1 stmt1) (Labeled i2 stmt2) =
    eq opt i1 i2 && eq opt stmt1 stmt2
  eq _ _ _ = False

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch = Catch FormalParam Block
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Catch where
  eq opt (Catch fp1 b1) (Catch fp2 b2) =
    eq opt fp1 fp2 && eq opt b1 b2

data TryResource
  = TryResourceVarDecl ResourceDecl
  | TryResourceVarAccess Ident
  | TryResourceQualAccess FieldAccess
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality TryResource where
  eq opt (TryResourceVarDecl rd1) (TryResourceVarDecl rd2) =
    eq opt rd1 rd2
  eq opt (TryResourceVarAccess i1) (TryResourceVarAccess i2) =
    eq opt i1 i2
  eq opt (TryResourceQualAccess fa1) (TryResourceQualAccess fa2) =
    eq opt fa1 fa2
  eq _ _ _ = False

data ResourceDecl
  = ResourceDecl [Modifier] Type VarDeclId VarInit
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ResourceDecl where
  eq opt (ResourceDecl ms1 t1 vdi1 vi1) (ResourceDecl ms2 t2 vdi2 vi2) =
    eq opt ms1 ms2 && eq opt t1 t2 && eq opt vdi1 vdi2 && eq opt vi1 vi2

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock
  = SwitchBlock SourceSpan SwitchLabel [BlockStmt]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality SwitchBlock where
  eq opt (SwitchBlock s1 sl1 bss1) (SwitchBlock s2 sl2 bss2) =
    eq opt s1 s2 && eq opt sl1 sl2 && eq opt bss1 bss2

data SwitchStyle
  = SwitchOldStyle
  | SwitchNewStyle -- JEP 361
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality SwitchStyle where
  eq _ SwitchOldStyle SwitchOldStyle = True
  eq _ SwitchNewStyle SwitchNewStyle = True
  eq _ _ _ = False

-- | A label within a @switch@ statement.
data SwitchLabel
  = -- | The expressions contained in the @case@ must be a 'Lit' or an @enum@ constant.
    -- The list must be non-empty.
    SwitchCase [Exp]
  | Default
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality SwitchLabel where
  eq opt (SwitchCase es1) (SwitchCase es2) =
    eq opt es1 es2
  eq _ Default Default = True
  eq _ _ _ = False

data SwitchExpBranch
  = SwitchExpBranch SwitchLabel SwitchExpBranchBody
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality SwitchExpBranch where
  eq opt (SwitchExpBranch sl1 sebb1) (SwitchExpBranch sl2 sebb2) =
    eq opt sl1 sl2 && eq opt sebb1 sebb2

data SwitchExpBranchBody
  = SwitchExpBranchExp Exp
  | SwitchExpBranchBlock [BlockStmt]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality SwitchExpBranchBody where
  eq opt (SwitchExpBranchExp e1) (SwitchExpBranchExp e2) =
    eq opt e1 e2
  eq opt (SwitchExpBranchBlock bss1) (SwitchExpBranchBlock bss2) =
    eq opt bss1 bss2
  eq _ _ _ = False

-- | Initialization code for a basic @for@ statement.
data ForInit
  = ForLocalVars [Modifier] Type [VarDecl]
  | ForInitExps [Exp]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ForInit where
  eq opt (ForLocalVars ms1 t1 vds1) (ForLocalVars ms2 t2 vds2) =
    eq opt ms1 ms2 && eq opt t1 t2 && eq opt vds1 vds2
  eq opt (ForInitExps es1) (ForInitExps es2) =
    eq opt es1 es2
  eq _ _ _ = False

-- | An exception type has to be a class type or a type variable.
type ExceptionType = RefType -- restricted to ClassType or TypeVariable

-- | Arguments to methods and constructors are expressions.
type Argument = Exp

-- | A Java expression.
data Exp
  = -- | A literal denotes a fixed, unchanging value.
    Lit Literal
  | -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    ClassLit (Maybe Type)
  | -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    This
  | -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    ThisClass Name
  | -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    InstanceCreation [TypeArgument] TypeDeclSpecifier [Argument] (Maybe ClassBody)
  | -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    QualInstanceCreation Exp [TypeArgument] Ident [Argument] (Maybe ClassBody)
  | -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    ArrayCreate Type [Exp] Int
  | -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    ArrayCreateInit Type Int ArrayInit
  | -- | A field access expression.
    FieldAccess FieldAccess
  | -- | A method invocation expression.
    MethodInv MethodInvocation
  | -- | An array access expression refers to a variable that is a component of an array.
    ArrayAccess ArrayIndex
  | {-    | ArrayAccess Exp Exp -- Should this be made into a datatype, for consistency and use with Lhs? -}

    -- | An expression name, e.g. a variable.
    ExpName Name
  | -- | Post-incrementation expression, i.e. an expression followed by @++@.
    PostIncrement SourceSpan Exp
  | -- | Post-decrementation expression, i.e. an expression followed by @--@.
    PostDecrement SourceSpan Exp
  | -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    PreIncrement SourceSpan Exp
  | -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    PreDecrement SourceSpan Exp
  | -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    PrePlus Exp
  | -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    PreMinus Exp
  | -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    PreBitCompl Exp
  | -- | Logical complementation of boolean values.
    PreNot Exp
  | -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    Cast Type Exp
  | -- | The application of a binary operator to two operand expressions.
    BinOp Exp Op Exp
  | -- | Testing whether the result of an expression is an instance of some reference type.
    InstanceOf Exp RefType (Maybe Name)
  | -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    Cond SourceSpan Exp Exp Exp
  | -- | Assignment of the result of an expression to a variable.
    Assign SourceSpan Lhs AssignOp Exp
  | -- | Lambda expression
    Lambda LambdaParams LambdaExpression
  | -- | Method reference
    MethodRef Name MethodRefTarget
  | -- | New-style switch expression (JEP 361)
    SwitchExp Exp [SwitchExpBranch]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Exp where
  eq opt (Lit l1) (Lit l2) =
    eq opt l1 l2
  eq opt (ClassLit mt1) (ClassLit mt2) =
    eq opt mt1 mt2
  eq _ This This = True
  eq opt (ThisClass n1) (ThisClass n2) =
    eq opt n1 n2
  eq opt (InstanceCreation tas1 tds1 as1 mcb1) (InstanceCreation tas2 tds2 as2 mcb2) =
    eq opt tas1 tas2 && eq opt tds1 tds2 && eq opt as1 as2 && eq opt mcb1 mcb2
  eq opt (QualInstanceCreation e1 tas1 i1 as1 mcb1) (QualInstanceCreation e2 tas2 i2 as2 mcb2) =
    eq opt e1 e2 && eq opt tas1 tas2 && eq opt i1 i2 && eq opt as1 as2 && eq opt mcb1 mcb2
  eq opt (ArrayCreate t1 es1 int1) (ArrayCreate t2 es2 int2) =
    eq opt t1 t2 && eq opt es1 es2 && int1 == int2
  eq opt (ArrayCreateInit t1 int1 ai1) (ArrayCreateInit t2 int2 ai2) =
    eq opt t1 t2 && eq opt ai1 ai2 && int1 == int2
  eq opt (FieldAccess fa1) (FieldAccess fa2) =
    eq opt fa1 fa2
  eq opt (MethodInv mi1) (MethodInv mi2) =
    eq opt mi1 mi2
  eq opt (ArrayAccess ai1) (ArrayAccess ai2) =
    eq opt ai1 ai2
  eq opt (ExpName n1) (ExpName n2) =
    eq opt n1 n2
  eq opt (PostIncrement s1 e1) (PostIncrement s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PostDecrement s1 e1) (PostDecrement s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PreIncrement s1 e1) (PreIncrement s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PreDecrement s1 e1) (PreDecrement s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PrePlus e1) (PrePlus e2) =
    eq opt e1 e2
  eq opt (PreMinus e1) (PreMinus e2) =
    eq opt e1 e2
  eq opt (PreBitCompl e1) (PreBitCompl e2) =
    eq opt e1 e2
  eq opt (PreNot e1) (PreNot e2) =
    eq opt e1 e2
  eq opt (Cast t1 e1) (Cast t2 e2) =
    eq opt t1 t2 && eq opt e1 e2
  eq opt (BinOp e11 o1 e12) (BinOp e21 o2 e22) =
    eq opt e11 e21 && eq opt o1 o2 && eq opt e12 e22
  eq opt (InstanceOf e1 rt1 mn1) (InstanceOf e2 rt2 mn2) =
    eq opt e1 e2 && eq opt rt1 rt2 && eq opt mn1 mn2
  eq opt (Cond s1 e11 e12 e13) (Cond s2 e21 e22 e23) =
    eq opt s1 s2 && eq opt e11 e21 && eq opt e12 e22 && eq opt e13 e23
  eq opt (Assign s1 lhs1 ao1 e1) (Assign s2 lhs2 ao2 e2) =
    eq opt s1 s2 && eq opt lhs1 lhs2 && eq opt ao1 ao2 && eq opt e1 e2
  eq opt (Lambda lp1 le1) (Lambda lp2 le2) =
    eq opt lp1 lp2 && eq opt le1 le2
  eq opt (MethodRef n1 mrt1) (MethodRef n2 mrt2) =
    eq opt n1 n2 && eq opt mrt1 mrt2
  eq opt (SwitchExp e1 sebs1) (SwitchExp e2 sebs2) =
    eq opt e1 e2 && eq opt sebs1 sebs2
  eq _ _ _ = False

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data Lhs
  = -- | Assign to a variable
    NameLhs Name
  | -- | Assign through a field access
    FieldLhs FieldAccess
  | -- | Assign to an array
    ArrayLhs ArrayIndex
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Lhs where
  eq opt (NameLhs n1) (NameLhs n2) =
    eq opt n1 n2
  eq opt (FieldLhs fa1) (FieldLhs fa2) =
    eq opt fa1 fa2
  eq opt (ArrayLhs ai1) (ArrayLhs ai2) =
    eq opt ai1 ai2
  eq _ _ _ = False

-- | Array access
data ArrayIndex
  = -- | Index into an array
    ArrayIndex Exp [Exp]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ArrayIndex where
  eq opt (ArrayIndex e1 es1) (ArrayIndex e2 es2) =
    eq opt e1 e2 && eq opt es1 es2

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess
  = -- | Accessing a field of an object or array computed from an expression.
    PrimaryFieldAccess Exp Ident
  | -- | Accessing a field of the superclass.
    SuperFieldAccess Ident
  | -- | Accessing a (static) field of a named class.
    ClassFieldAccess Name Ident
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality FieldAccess where
  eq opt (PrimaryFieldAccess e1 i1) (PrimaryFieldAccess e2 i2) =
    eq opt e1 e2 && eq opt i1 i2
  eq opt (SuperFieldAccess i1) (SuperFieldAccess i2) =
    eq opt i1 i2
  eq opt (ClassFieldAccess n1 i1) (ClassFieldAccess n2 i2) =
    eq opt n1 n2 && eq opt i1 i2
  eq _ _ _ = False

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParams
  = LambdaSingleParam Ident
  | LambdaFormalParams [FormalParam]
  | LambdaInferredParams [Ident]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality LambdaParams where
  eq opt (LambdaSingleParam i1) (LambdaSingleParam i2) =
    eq opt i1 i2
  eq opt (LambdaFormalParams fps1) (LambdaFormalParams fps2) =
    eq opt fps1 fps2
  eq opt (LambdaInferredParams is1) (LambdaInferredParams is2) =
    eq opt is1 is2
  eq _ _ _ = False

-- | Lambda expression, starting from java 8
data LambdaExpression
  = LambdaExpression Exp
  | LambdaBlock Block
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality LambdaExpression where
  eq opt (LambdaExpression e1) (LambdaExpression e2) =
    eq opt e1 e2
  eq opt (LambdaBlock b1) (LambdaBlock b2) =
    eq opt b1 b2
  eq _ _ _ = False

-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocation
  = -- | Invoking a specific named method.
    MethodCall Name [Argument]
  | -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    PrimaryMethodCall Exp [RefType] Ident [Argument]
  | -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    SuperMethodCall [RefType] Ident [Argument]
  | -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    ClassMethodCall Name [RefType] Ident [Argument]
  | -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    TypeMethodCall Name [RefType] Ident [Argument]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality MethodInvocation where
  eq opt (MethodCall n1 as1) (MethodCall n2 as2) =
    eq opt n1 n2 && eq opt as1 as2
  eq opt (PrimaryMethodCall e1 rts1 i1 as1) (PrimaryMethodCall e2 rts2 i2 as2) =
    eq opt e1 e2 && eq opt rts1 rts2 && eq opt i1 i2 && eq opt as1 as2
  eq opt (SuperMethodCall rts1 i1 as1) (SuperMethodCall rts2 i2 as2) =
    eq opt rts1 rts2 && eq opt i1 i2 && eq opt as1 as2
  eq opt (ClassMethodCall n1 rts1 i1 as1) (ClassMethodCall n2 rts2 i2 as2) =
    eq opt n1 n2 && eq opt rts1 rts2 && eq opt i1 i2 && eq opt as1 as2
  eq opt (TypeMethodCall n1 rts1 i1 as1) (TypeMethodCall n2 rts2 i2 as2) =
    eq opt n1 n2 && eq opt rts1 rts2 && eq opt i1 i2 && eq opt as1 as2
  eq _ _ _ = False

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
newtype ArrayInit
  = ArrayInit [VarInit]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ArrayInit where
  eq opt (ArrayInit vis1) (ArrayInit vis2) =
    eq opt vis1 vis2

data MethodRefTarget
  = MethodRefIdent Ident
  | MethodRefConstructor
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality MethodRefTarget where
  eq opt (MethodRefIdent i1) (MethodRefIdent i2) =
    eq opt i1 i2
  eq _ MethodRefConstructor MethodRefConstructor = True
  eq _ _ _ = False
