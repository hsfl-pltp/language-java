{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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
    Parsed,
    Analyzed,
  )
where

import Data.Data
import GHC.Generics (Generic)
import Language.Java.SourceSpan
import Language.Java.Syntax.Exp
import Language.Java.Syntax.Types

-----------------------------------------------------------------------
-- AST types

data Parsed
  deriving (Data)

data Analyzed
  deriving (Data)

-----------------------------------------------------------------------
-- Packages

-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit p = CompilationUnit (Maybe (PackageDecl p)) [ImportDecl p] [TypeDecl p]
  deriving (Typeable, Generic)

deriving instance Eq (CompilationUnit Analyzed)

deriving instance Eq (CompilationUnit Parsed)

deriving instance Show (CompilationUnit Analyzed)

deriving instance Show (CompilationUnit Parsed)

deriving instance Read (CompilationUnit Analyzed)

deriving instance Read (CompilationUnit Parsed)

deriving instance Data (CompilationUnit Analyzed)

deriving instance Data (CompilationUnit Parsed)

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
newtype PackageDecl p = PackageDecl Name
  deriving (Typeable, Generic)

deriving instance Eq (PackageDecl Analyzed)

deriving instance Eq (PackageDecl Parsed)

deriving instance Show (PackageDecl Analyzed)

deriving instance Show (PackageDecl Parsed)

deriving instance Read (PackageDecl Analyzed)

deriving instance Read (PackageDecl Parsed)

deriving instance Data (PackageDecl Analyzed)

deriving instance Data (PackageDecl Parsed)

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl p
  = ImportDecl SourceSpan Bool {- static? -} Name Bool {- .*? -}
  deriving (Typeable, Generic)

deriving instance Eq (ImportDecl Analyzed)

deriving instance Eq (ImportDecl Parsed)

deriving instance Show (ImportDecl Analyzed)

deriving instance Show (ImportDecl Parsed)

deriving instance Read (ImportDecl Analyzed)

deriving instance Read (ImportDecl Parsed)

deriving instance Data (ImportDecl Analyzed)

deriving instance Data (ImportDecl Parsed)

-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl p
  = ClassTypeDecl (ClassDecl p)
  | InterfaceTypeDecl (InterfaceDecl p)
  deriving (Typeable, Generic)

deriving instance Eq (TypeDecl Analyzed)

deriving instance Eq (TypeDecl Parsed)

deriving instance Show (TypeDecl Analyzed)

deriving instance Show (TypeDecl Parsed)

deriving instance Read (TypeDecl Analyzed)

deriving instance Read (TypeDecl Parsed)

deriving instance Data (TypeDecl Analyzed)

deriving instance Data (TypeDecl Parsed)

-- | A class declaration specifies a new named reference type.
data ClassDecl p
  = ClassDecl SourceSpan [Modifier p] Ident [TypeParam] (Maybe RefType) [RefType] (ClassBody p)
  | RecordDecl SourceSpan [Modifier p] Ident [TypeParam] [RecordFieldDecl] [RefType] (ClassBody p)
  | EnumDecl SourceSpan [Modifier p] Ident [RefType] (EnumBody p)
  deriving (Typeable, Generic)

deriving instance Eq (ClassDecl Analyzed)

deriving instance Eq (ClassDecl Parsed)

deriving instance Show (ClassDecl Analyzed)

deriving instance Show (ClassDecl Parsed)

deriving instance Read (ClassDecl Analyzed)

deriving instance Read (ClassDecl Parsed)

deriving instance Data (ClassDecl Analyzed)

deriving instance Data (ClassDecl Parsed)

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
newtype ClassBody p = ClassBody [Decl p]
  deriving (Typeable, Generic)

deriving instance Eq (ClassBody Analyzed)

deriving instance Eq (ClassBody Parsed)

deriving instance Show (ClassBody Analyzed)

deriving instance Show (ClassBody Parsed)

deriving instance Read (ClassBody Analyzed)

deriving instance Read (ClassBody Parsed)

deriving instance Data (ClassBody Analyzed)

deriving instance Data (ClassBody Parsed)

-- | The body of an enum type may contain enum constants.
data EnumBody p = EnumBody [EnumConstant p] [Decl p]
  deriving (Typeable, Generic)

deriving instance Eq (EnumBody Analyzed)

deriving instance Eq (EnumBody Parsed)

deriving instance Show (EnumBody Analyzed)

deriving instance Show (EnumBody Parsed)

deriving instance Read (EnumBody Analyzed)

deriving instance Read (EnumBody Parsed)

deriving instance Data (EnumBody Analyzed)

deriving instance Data (EnumBody Parsed)

-- | An enum constant defines an instance of the enum type.
data EnumConstant p = EnumConstant Ident [Argument p] (Maybe (ClassBody p))
  deriving (Typeable, Generic)

deriving instance Eq (EnumConstant Analyzed)

deriving instance Eq (EnumConstant Parsed)

deriving instance Show (EnumConstant Analyzed)

deriving instance Show (EnumConstant Parsed)

deriving instance Read (EnumConstant Analyzed)

deriving instance Read (EnumConstant Parsed)

deriving instance Data (EnumConstant Analyzed)

deriving instance Data (EnumConstant Parsed)

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl p
  = InterfaceDecl SourceSpan InterfaceKind [Modifier p] Ident [TypeParam] [RefType {- extends -}] [RefType {- permits -}] (InterfaceBody p)
  deriving (Typeable, Generic)

deriving instance Eq (InterfaceDecl Analyzed)

deriving instance Eq (InterfaceDecl Parsed)

deriving instance Show (InterfaceDecl Analyzed)

deriving instance Show (InterfaceDecl Parsed)

deriving instance Read (InterfaceDecl Analyzed)

deriving instance Read (InterfaceDecl Parsed)

deriving instance Data (InterfaceDecl Analyzed)

deriving instance Data (InterfaceDecl Parsed)

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | The body of an interface may declare members of the interface.
newtype InterfaceBody p
  = InterfaceBody [MemberDecl p]
  deriving (Typeable, Generic)

deriving instance Eq (InterfaceBody Analyzed)

deriving instance Eq (InterfaceBody Parsed)

deriving instance Show (InterfaceBody Analyzed)

deriving instance Show (InterfaceBody Parsed)

deriving instance Read (InterfaceBody Analyzed)

deriving instance Read (InterfaceBody Parsed)

deriving instance Data (InterfaceBody Analyzed)

deriving instance Data (InterfaceBody Parsed)

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl p
  = MemberDecl (MemberDecl p)
  | InitDecl Bool (Block p)
  deriving (Typeable, Generic)

deriving instance Eq (Decl Analyzed)

deriving instance Eq (Decl Parsed)

deriving instance Show (Decl Analyzed)

deriving instance Show (Decl Parsed)

deriving instance Read (Decl Analyzed)

deriving instance Read (Decl Parsed)

deriving instance Data (Decl Analyzed)

deriving instance Data (Decl Parsed)

-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl p
  = -- | The variables of a class type are introduced by field declarations.
    FieldDecl SourceSpan [Modifier p] Type [VarDecl p]
  | -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    MethodDecl SourceSpan [Modifier p] [TypeParam] (Maybe Type) Ident [FormalParam p] [ExceptionType] (Maybe (Exp p)) (MethodBody p)
  | -- | A constructor is used in the creation of an object that is an instance of a class.
    ConstructorDecl SourceSpan [Modifier p] [TypeParam] Ident [FormalParam p] [ExceptionType] (ConstructorBody p)
  | -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    MemberClassDecl (ClassDecl p)
  | -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    MemberInterfaceDecl (InterfaceDecl p)
  deriving (Typeable, Generic)

deriving instance Eq (MemberDecl Analyzed)

deriving instance Eq (MemberDecl Parsed)

deriving instance Show (MemberDecl Analyzed)

deriving instance Show (MemberDecl Parsed)

deriving instance Read (MemberDecl Analyzed)

deriving instance Read (MemberDecl Parsed)

deriving instance Data (MemberDecl Analyzed)

deriving instance Data (MemberDecl Parsed)

-- | A field declaration of a record
data RecordFieldDecl
  = RecordFieldDecl Type Ident
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl p
  = VarDecl SourceSpan VarDeclId (Maybe (VarInit p))
  deriving (Typeable, Generic)

deriving instance Eq (VarDecl Analyzed)

deriving instance Eq (VarDecl Parsed)

deriving instance Show (VarDecl Analyzed)

deriving instance Show (VarDecl Parsed)

deriving instance Read (VarDecl Analyzed)

deriving instance Read (VarDecl Parsed)

deriving instance Data (VarDecl Analyzed)

deriving instance Data (VarDecl Parsed)

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId
  = VarId Ident
  | -- | Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
    VarDeclArray SourceSpan VarDeclId
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | Explicit initializer for a variable declaration.
data VarInit p
  = InitExp (Exp p)
  | InitArray (ArrayInit p)
  deriving (Typeable, Generic)

deriving instance Eq (VarInit Analyzed)

deriving instance Eq (VarInit Parsed)

deriving instance Show (VarInit Analyzed)

deriving instance Show (VarInit Parsed)

deriving instance Read (VarInit Analyzed)

deriving instance Read (VarInit Parsed)

deriving instance Data (VarInit Analyzed)

deriving instance Data (VarInit Parsed)

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam p = FormalParam SourceSpan [Modifier p] Type Bool VarDeclId
  deriving (Typeable, Generic)

deriving instance Eq (FormalParam Analyzed)

deriving instance Eq (FormalParam Parsed)

deriving instance Show (FormalParam Analyzed)

deriving instance Show (FormalParam Parsed)

deriving instance Read (FormalParam Analyzed)

deriving instance Read (FormalParam Parsed)

deriving instance Data (FormalParam Analyzed)

deriving instance Data (FormalParam Parsed)

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
newtype MethodBody p = MethodBody (Maybe (Block p))
  deriving (Typeable, Generic)

deriving instance Eq (MethodBody Analyzed)

deriving instance Eq (MethodBody Parsed)

deriving instance Show (MethodBody Analyzed)

deriving instance Show (MethodBody Parsed)

deriving instance Read (MethodBody Analyzed)

deriving instance Read (MethodBody Parsed)

deriving instance Data (MethodBody Analyzed)

deriving instance Data (MethodBody Parsed)

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody p = ConstructorBody (Maybe (ExplConstrInv p)) [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance Eq (ConstructorBody Analyzed)

deriving instance Eq (ConstructorBody Parsed)

deriving instance Show (ConstructorBody Analyzed)

deriving instance Show (ConstructorBody Parsed)

deriving instance Read (ConstructorBody Analyzed)

deriving instance Read (ConstructorBody Parsed)

deriving instance Data (ConstructorBody Analyzed)

deriving instance Data (ConstructorBody Parsed)

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv p
  = ThisInvoke [RefType] [Argument p]
  | SuperInvoke [RefType] [Argument p]
  | PrimarySuperInvoke (Exp p) [RefType] [Argument p]
  deriving (Typeable, Generic)

deriving instance Eq (ExplConstrInv Analyzed)

deriving instance Eq (ExplConstrInv Parsed)

deriving instance Show (ExplConstrInv Analyzed)

deriving instance Show (ExplConstrInv Parsed)

deriving instance Read (ExplConstrInv Analyzed)

deriving instance Read (ExplConstrInv Parsed)

deriving instance Data (ExplConstrInv Analyzed)

deriving instance Data (ExplConstrInv Parsed)

-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier p
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
  | Annotation (Annotation p)
  | Synchronized_
  | Sealed
  deriving (Typeable, Generic)

instance Show (Modifier Analyzed) where
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

instance Show (Modifier Parsed) where
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

deriving instance Eq (Modifier Analyzed)

deriving instance Eq (Modifier Parsed)

deriving instance Read (Modifier Analyzed)

deriving instance Read (Modifier Parsed)

deriving instance Data (Modifier Analyzed)

deriving instance Data (Modifier Parsed)

-- | Annotations have three different forms: no-parameter, single-parameter or key-value pairs
data Annotation p
  = NormalAnnotation
      { span :: SourceSpan,
        annName :: Name, -- Not type because not type generics not allowed
        annKV :: [(Ident, ElementValue p)]
      }
  | SingleElementAnnotation
      { span :: SourceSpan,
        annName :: Name,
        annValue :: ElementValue p
      }
  | MarkerAnnotation
      { span :: SourceSpan,
        annName :: Name
      }
  deriving (Typeable, Generic)

deriving instance Eq (Annotation Analyzed)

deriving instance Eq (Annotation Parsed)

deriving instance Show (Annotation Analyzed)

deriving instance Show (Annotation Parsed)

deriving instance Read (Annotation Analyzed)

deriving instance Read (Annotation Parsed)

deriving instance Data (Annotation Analyzed)

deriving instance Data (Annotation Parsed)

desugarAnnotation (MarkerAnnotation span n) = (span, n, [])
-- TODO: check span for ident
desugarAnnotation (SingleElementAnnotation span n e) = (span, n, [(Ident dummySourceSpan "value", e)])
desugarAnnotation (NormalAnnotation span n kv) = (span, n, kv)

desugarAnnotation' (MarkerAnnotation span n) = NormalAnnotation span n []
-- TODO: check span for ident
desugarAnnotation' (SingleElementAnnotation span n e) = NormalAnnotation span n [(Ident dummySourceSpan "value", e)]
desugarAnnotation' normal = normal

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue p
  = EVVal (VarInit p)
  | EVAnn (Annotation p)
  deriving (Typeable, Generic)

deriving instance Eq (ElementValue Analyzed)

deriving instance Eq (ElementValue Parsed)

deriving instance Show (ElementValue Analyzed)

deriving instance Show (ElementValue Parsed)

deriving instance Read (ElementValue Analyzed)

deriving instance Read (ElementValue Parsed)

deriving instance Data (ElementValue Analyzed)

deriving instance Data (ElementValue Parsed)

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
newtype Block p = Block [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance Eq (Block Analyzed)

deriving instance Eq (Block Parsed)

deriving instance Show (Block Analyzed)

deriving instance Show (Block Parsed)

deriving instance Read (Block Analyzed)

deriving instance Read (Block Parsed)

deriving instance Data (Block Analyzed)

deriving instance Data (Block Parsed)

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt p
  = BlockStmt SourceSpan (Stmt p)
  | LocalClass (ClassDecl p)
  | LocalVars SourceSpan [Modifier p] Type [VarDecl p]
  deriving (Typeable, Generic)

deriving instance Eq (BlockStmt Analyzed)

deriving instance Eq (BlockStmt Parsed)

deriving instance Show (BlockStmt Analyzed)

deriving instance Show (BlockStmt Parsed)

deriving instance Read (BlockStmt Analyzed)

deriving instance Read (BlockStmt Parsed)

deriving instance Data (BlockStmt Analyzed)

deriving instance Data (BlockStmt Parsed)

-- | A Java statement.
data Stmt p
  = -- | A statement can be a nested block.
    StmtBlock (Block p)
  | -- | The @if-then@ statement allows conditional execution of a statement.
    IfThen SourceSpan (Exp p) (Stmt p)
  | -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    IfThenElse SourceSpan (Exp p) (Stmt p) (Stmt p)
  | -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    While SourceSpan (Exp p) (Stmt p)
  | -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    BasicFor SourceSpan (Maybe (ForInit p)) (Maybe (Exp p)) (Maybe [Exp p]) (Stmt p)
  | -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    EnhancedFor SourceSpan [Modifier p] Type Ident (Exp p) (Stmt p)
  | -- | An empty statement does nothing.
    Empty
  | -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    ExpStmt SourceSpan (Exp p)
  | -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    Assert (Exp p) (Maybe (Exp p))
  | -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    Switch SwitchStyle (Exp p) [SwitchBlock p]
  | -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    Do SourceSpan (Stmt p) (Exp p)
  | -- | A @break@ statement transfers control out of an enclosing statement.
    Break SourceSpan (Maybe Ident)
  | -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    Continue (Maybe Ident)
  | -- A @return@ statement returns control to the invoker of a method or constructor.
    Return SourceSpan (Maybe (Exp p))
  | -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    Synchronized (Exp p) (Block p)
  | -- | A @throw@ statement causes an exception to be thrown.
    Throw (Exp p)
  | -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    Try SourceSpan [TryResource p] (Block p) [Catch p] (Maybe {- finally -} (Block p))
  | -- | Statements may have label prefixes.
    Labeled Ident (Stmt p)
  deriving (Generic, Typeable)

deriving instance Eq (Stmt Analyzed)

deriving instance Eq (Stmt Parsed)

deriving instance Show (Stmt Analyzed)

deriving instance Show (Stmt Parsed)

deriving instance Read (Stmt Analyzed)

deriving instance Read (Stmt Parsed)

deriving instance Data (Stmt Analyzed)

deriving instance Data (Stmt Parsed)

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch p = Catch (FormalParam p) (Block p)
  deriving (Typeable, Generic)

deriving instance Eq (Catch Analyzed)

deriving instance Eq (Catch Parsed)

deriving instance Show (Catch Analyzed)

deriving instance Show (Catch Parsed)

deriving instance Read (Catch Analyzed)

deriving instance Read (Catch Parsed)

deriving instance Data (Catch Analyzed)

deriving instance Data (Catch Parsed)

data TryResource p
  = TryResourceVarDecl (ResourceDecl p)
  | TryResourceVarAccess Ident
  | TryResourceQualAccess (FieldAccess p)
  deriving (Typeable, Generic)

deriving instance Eq (TryResource Analyzed)

deriving instance Eq (TryResource Parsed)

deriving instance Show (TryResource Analyzed)

deriving instance Show (TryResource Parsed)

deriving instance Read (TryResource Analyzed)

deriving instance Read (TryResource Parsed)

deriving instance Data (TryResource Analyzed)

deriving instance Data (TryResource Parsed)

data ResourceDecl p
  = ResourceDecl [Modifier p] Type VarDeclId (VarInit p)
  deriving (Typeable, Generic)

deriving instance Eq (ResourceDecl Analyzed)

deriving instance Eq (ResourceDecl Parsed)

deriving instance Show (ResourceDecl Analyzed)

deriving instance Show (ResourceDecl Parsed)

deriving instance Read (ResourceDecl Analyzed)

deriving instance Read (ResourceDecl Parsed)

deriving instance Data (ResourceDecl Analyzed)

deriving instance Data (ResourceDecl Parsed)

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock p
  = SwitchBlock SourceSpan (SwitchLabel p) [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance Eq (SwitchBlock Analyzed)

deriving instance Eq (SwitchBlock Parsed)

deriving instance Show (SwitchBlock Analyzed)

deriving instance Show (SwitchBlock Parsed)

deriving instance Read (SwitchBlock Analyzed)

deriving instance Read (SwitchBlock Parsed)

deriving instance Data (SwitchBlock Analyzed)

deriving instance Data (SwitchBlock Parsed)

data SwitchStyle
  = SwitchOldStyle
  | SwitchNewStyle -- JEP 361
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | A label within a @switch@ statement.
data SwitchLabel p
  = -- | The expressions contained in the @case@ must be a 'Lit' or an @enum@ constant.
    -- The list must be non-empty.
    SwitchCase [Exp p]
  | Default
  deriving (Typeable, Generic)

deriving instance Eq (SwitchLabel Analyzed)

deriving instance Eq (SwitchLabel Parsed)

deriving instance Show (SwitchLabel Analyzed)

deriving instance Show (SwitchLabel Parsed)

deriving instance Read (SwitchLabel Analyzed)

deriving instance Read (SwitchLabel Parsed)

deriving instance Data (SwitchLabel Analyzed)

deriving instance Data (SwitchLabel Parsed)

data SwitchExpBranch p
  = SwitchExpBranch (SwitchLabel p) (SwitchExpBranchBody p)
  deriving (Typeable, Generic)

deriving instance Eq (SwitchExpBranch Analyzed)

deriving instance Eq (SwitchExpBranch Parsed)

deriving instance Show (SwitchExpBranch Analyzed)

deriving instance Show (SwitchExpBranch Parsed)

deriving instance Read (SwitchExpBranch Analyzed)

deriving instance Read (SwitchExpBranch Parsed)

deriving instance Data (SwitchExpBranch Analyzed)

deriving instance Data (SwitchExpBranch Parsed)

data SwitchExpBranchBody p
  = SwitchExpBranchExp (Exp p)
  | SwitchExpBranchBlock [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance Eq (SwitchExpBranchBody Analyzed)

deriving instance Eq (SwitchExpBranchBody Parsed)

deriving instance Show (SwitchExpBranchBody Analyzed)

deriving instance Show (SwitchExpBranchBody Parsed)

deriving instance Read (SwitchExpBranchBody Analyzed)

deriving instance Read (SwitchExpBranchBody Parsed)

deriving instance Data (SwitchExpBranchBody Analyzed)

deriving instance Data (SwitchExpBranchBody Parsed)

-- | Initialization code for a basic @for@ statement.
data ForInit p
  = ForLocalVars [Modifier p] Type [VarDecl p]
  | ForInitExps [Exp p]
  deriving (Generic, Typeable)

deriving instance Eq (ForInit Analyzed)

deriving instance Eq (ForInit Parsed)

deriving instance Show (ForInit Analyzed)

deriving instance Show (ForInit Parsed)

deriving instance Read (ForInit Analyzed)

deriving instance Read (ForInit Parsed)

deriving instance Data (ForInit Analyzed)

deriving instance Data (ForInit Parsed)

-- | An exception type has to be a class type or a type variable.
type ExceptionType = RefType -- restricted to ClassType or TypeVariable

-- | Arguments to methods and constructors are expressions.
type Argument p = Exp p

-- | A Java expression.
data Exp p
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
    InstanceCreation [TypeArgument] TypeDeclSpecifier [Argument p] (Maybe (ClassBody p))
  | -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    QualInstanceCreation (Exp p) [TypeArgument] Ident [Argument p] (Maybe (ClassBody p))
  | -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    ArrayCreate Type [Exp p] Int
  | -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    ArrayCreateInit Type Int (ArrayInit p)
  | -- | A field access expression.
    FieldAccess (FieldAccess p)
  | -- | A method invocation expression.
    MethodInv (MethodInvocation p)
  | -- | An array access expression refers to a variable that is a component of an array.
    ArrayAccess (ArrayIndex p)
  | {-    | ArrayAccess Exp Exp -- Should this be made into a datatype, for consistency and use with Lhs? -}

    -- | An expression name, e.g. a variable.
    ExpName Name
  | -- | Post-incrementation expression, i.e. an expression followed by @++@.
    PostIncrement SourceSpan (Exp p)
  | -- | Post-decrementation expression, i.e. an expression followed by @--@.
    PostDecrement SourceSpan (Exp p)
  | -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    PreIncrement SourceSpan (Exp p)
  | -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    PreDecrement SourceSpan (Exp p)
  | -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    PrePlus (Exp p)
  | -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    PreMinus (Exp p)
  | -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    PreBitCompl (Exp p)
  | -- | Logical complementation of boolean values.
    PreNot (Exp p)
  | -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    Cast Type (Exp p)
  | -- | The application of a binary operator to two operand expressions.
    BinOp (Exp p) Op (Exp p)
  | -- | Testing whether the result of an expression is an instance of some reference type.
    InstanceOf (Exp p) RefType (Maybe Name)
  | -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    Cond SourceSpan (Exp p) (Exp p) (Exp p)
  | -- | Assignment of the result of an expression to a variable.
    Assign SourceSpan (Lhs p) AssignOp (Exp p)
  | -- | Lambda expression
    Lambda (LambdaParams p) (LambdaExpression p)
  | -- | Method reference
    MethodRef Name MethodRefTarget
  | -- | New-style switch expression (JEP 361)
    SwitchExp (Exp p) [SwitchExpBranch p]
  deriving (Typeable, Generic)

deriving instance Eq (Exp Analyzed)

deriving instance Eq (Exp Parsed)

deriving instance Show (Exp Analyzed)

deriving instance Show (Exp Parsed)

deriving instance Read (Exp Analyzed)

deriving instance Read (Exp Parsed)

deriving instance Data (Exp Analyzed)

deriving instance Data (Exp Parsed)

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data Lhs p
  = -- | Assign to a variable
    NameLhs Name
  | -- | Assign through a field access
    FieldLhs (FieldAccess p)
  | -- | Assign to an array
    ArrayLhs (ArrayIndex p)
  deriving (Typeable, Generic)

deriving instance Eq (Lhs Analyzed)

deriving instance Eq (Lhs Parsed)

deriving instance Show (Lhs Analyzed)

deriving instance Show (Lhs Parsed)

deriving instance Read (Lhs Analyzed)

deriving instance Read (Lhs Parsed)

deriving instance Data (Lhs Analyzed)

deriving instance Data (Lhs Parsed)

-- | Array access
data ArrayIndex p
  = -- | Index into an array
    ArrayIndex (Exp p) [Exp p]
  deriving (Typeable, Generic)

deriving instance Eq (ArrayIndex Analyzed)

deriving instance Eq (ArrayIndex Parsed)

deriving instance Show (ArrayIndex Analyzed)

deriving instance Show (ArrayIndex Parsed)

deriving instance Read (ArrayIndex Analyzed)

deriving instance Read (ArrayIndex Parsed)

deriving instance Data (ArrayIndex Analyzed)

deriving instance Data (ArrayIndex Parsed)

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess p
  = -- | Accessing a field of an object or array computed from an expression.
    PrimaryFieldAccess (Exp p) Ident
  | -- | Accessing a field of the superclass.
    SuperFieldAccess Ident
  | -- | Accessing a (static) field of a named class.
    ClassFieldAccess Name Ident
  deriving (Typeable, Generic)

deriving instance Eq (FieldAccess Analyzed)

deriving instance Eq (FieldAccess Parsed)

deriving instance Show (FieldAccess Analyzed)

deriving instance Show (FieldAccess Parsed)

deriving instance Read (FieldAccess Analyzed)

deriving instance Read (FieldAccess Parsed)

deriving instance Data (FieldAccess Analyzed)

deriving instance Data (FieldAccess Parsed)

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParams p
  = LambdaSingleParam Ident
  | LambdaFormalParams [FormalParam p]
  | LambdaInferredParams [Ident]
  deriving (Typeable, Generic)

deriving instance Eq (LambdaParams Analyzed)

deriving instance Eq (LambdaParams Parsed)

deriving instance Show (LambdaParams Analyzed)

deriving instance Show (LambdaParams Parsed)

deriving instance Read (LambdaParams Analyzed)

deriving instance Read (LambdaParams Parsed)

deriving instance Data (LambdaParams Analyzed)

deriving instance Data (LambdaParams Parsed)

-- | Lambda expression, starting from java 8
data LambdaExpression p
  = LambdaExpression (Exp p)
  | LambdaBlock (Block p)
  deriving (Typeable, Generic)

deriving instance Eq (LambdaExpression Analyzed)

deriving instance Eq (LambdaExpression Parsed)

deriving instance Show (LambdaExpression Analyzed)

deriving instance Show (LambdaExpression Parsed)

deriving instance Read (LambdaExpression Analyzed)

deriving instance Read (LambdaExpression Parsed)

deriving instance Data (LambdaExpression Analyzed)

deriving instance Data (LambdaExpression Parsed)

type family XNameClassification x where
  XNameClassification Analyzed = ClassifiedName
  XNameClassification Parsed = Name

-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocation p
  = -- | Invoking a specific named method.
    MethodCall (Maybe (XNameClassification p)) Ident [Argument p]
  | -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    PrimaryMethodCall (Exp p) [RefType] Ident [Argument p]
  | -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    SuperMethodCall [RefType] Ident [Argument p]
  | -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    ClassMethodCall Name [RefType] Ident [Argument p]
  | -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    TypeMethodCall Name [RefType] Ident [Argument p]
  deriving (Typeable, Generic)

deriving instance Eq (MethodInvocation Analyzed)

deriving instance Eq (MethodInvocation Parsed)

deriving instance Show (MethodInvocation Analyzed)

deriving instance Show (MethodInvocation Parsed)

deriving instance Read (MethodInvocation Analyzed)

deriving instance Read (MethodInvocation Parsed)

deriving instance Data (MethodInvocation Analyzed)

deriving instance Data (MethodInvocation Parsed)

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
newtype ArrayInit p
  = ArrayInit [VarInit p]
  deriving (Typeable, Generic)

deriving instance Eq (ArrayInit Analyzed)

deriving instance Eq (ArrayInit Parsed)

deriving instance Show (ArrayInit Analyzed)

deriving instance Show (ArrayInit Parsed)

deriving instance Read (ArrayInit Analyzed)

deriving instance Read (ArrayInit Parsed)

deriving instance Data (ArrayInit Analyzed)

deriving instance Data (ArrayInit Parsed)

data MethodRefTarget
  = MethodRefIdent Ident
  | MethodRefConstructor
  deriving (Eq, Show, Read, Typeable, Generic, Data)
