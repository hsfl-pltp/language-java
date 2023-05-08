{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
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
    XNameClassification,
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

class Eq (XNameClassification x) => EqExtension x

class Show (XNameClassification x) => ShowExtension x

class Read (XNameClassification x) => ReadExtension x

class (Data x, Data (XNameClassification x)) => DataExtension x

-----------------------------------------------------------------------
-- Packages

-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit p = CompilationUnit (Maybe (PackageDecl p)) [ImportDecl p] [TypeDecl p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (CompilationUnit p)

deriving instance ShowExtension p => Show (CompilationUnit p)

deriving instance ReadExtension p => Read (CompilationUnit p)

deriving instance DataExtension p => Data (CompilationUnit p)

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
newtype PackageDecl p = PackageDecl Name
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (PackageDecl p)

deriving instance ShowExtension p => Show (PackageDecl p)

deriving instance ReadExtension p => Read (PackageDecl p)

deriving instance DataExtension p => Data (PackageDecl p)

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl p
  = ImportDecl SourceSpan Bool {- static? -} Name Bool {- .*? -}
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (ImportDecl p)

deriving instance ShowExtension p => Show (ImportDecl p)

deriving instance ReadExtension p => Read (ImportDecl p)

deriving instance DataExtension p => Data (ImportDecl p)

-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl p
  = ClassTypeDecl (ClassDecl p)
  | InterfaceTypeDecl (InterfaceDecl p)
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (TypeDecl p)

deriving instance ShowExtension p => Show (TypeDecl p)

deriving instance ReadExtension p => Read (TypeDecl p)

deriving instance DataExtension p => Data (TypeDecl p)

-- | A class declaration specifies a new named reference type.
data ClassDecl p
  = ClassDecl SourceSpan [Modifier p] Ident [TypeParam] (Maybe RefType) [RefType] (ClassBody p)
  | RecordDecl SourceSpan [Modifier p] Ident [TypeParam] [RecordFieldDecl] [RefType] (ClassBody p)
  | EnumDecl SourceSpan [Modifier p] Ident [RefType] (EnumBody p)
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (ClassDecl p)

deriving instance ShowExtension p => Show (ClassDecl p)

deriving instance ReadExtension p => Read (ClassDecl p)

deriving instance DataExtension p => Data (ClassDecl p)

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
newtype ClassBody p = ClassBody [Decl p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (ClassBody p)

deriving instance ShowExtension p => Show (ClassBody p)

deriving instance ReadExtension p => Read (ClassBody p)

deriving instance DataExtension p => Data (ClassBody p)

-- | The body of an enum type may contain enum constants.
data EnumBody p = EnumBody [EnumConstant p] [Decl p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (EnumBody p)

deriving instance ShowExtension p => Show (EnumBody p)

deriving instance ReadExtension p => Read (EnumBody p)

deriving instance DataExtension p => Data (EnumBody p)

-- | An enum constant defines an instance of the enum type.
data EnumConstant p = EnumConstant Ident [Argument p] (Maybe (ClassBody p))
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (EnumConstant p)

deriving instance ShowExtension p => Show (EnumConstant p)

deriving instance ReadExtension p => Read (EnumConstant p)

deriving instance DataExtension p => Data (EnumConstant p)

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl p
  = InterfaceDecl SourceSpan InterfaceKind [Modifier p] Ident [TypeParam] [RefType {- extends -}] [RefType {- permits -}] (InterfaceBody p)
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (InterfaceDecl p)

deriving instance ShowExtension p => Show (InterfaceDecl p)

deriving instance ReadExtension p => Read (InterfaceDecl p)

deriving instance DataExtension p => Data (InterfaceDecl p)

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | The body of an interface may declare members of the interface.
newtype InterfaceBody p
  = InterfaceBody [MemberDecl p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (InterfaceBody p)

deriving instance ShowExtension p => Show (InterfaceBody p)

deriving instance ReadExtension p => Read (InterfaceBody p)

deriving instance DataExtension p => Data (InterfaceBody p)

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl p
  = MemberDecl (MemberDecl p)
  | InitDecl Bool (Block p)
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (Decl p)

deriving instance ShowExtension p => Show (Decl p)

deriving instance ReadExtension p => Read (Decl p)

deriving instance DataExtension p => Data (Decl p)

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

deriving instance EqExtension p => Eq (MemberDecl p)

deriving instance ShowExtension p => Show (MemberDecl p)

deriving instance ReadExtension p => Read (MemberDecl p)

deriving instance DataExtension p => Data (MemberDecl p)

-- | A field declaration of a record
data RecordFieldDecl
  = RecordFieldDecl Type Ident
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl p
  = VarDecl SourceSpan VarDeclId (Maybe (VarInit p))
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (VarDecl p)

deriving instance ShowExtension p => Show (VarDecl p)

deriving instance ReadExtension p => Read (VarDecl p)

deriving instance DataExtension p => Data (VarDecl p)

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

deriving instance EqExtension p => Eq (VarInit p)

deriving instance ShowExtension p => Show (VarInit p)

deriving instance ReadExtension p => Read (VarInit p)

deriving instance DataExtension p => Data (VarInit p)

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam p = FormalParam SourceSpan [Modifier p] Type Bool VarDeclId
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (FormalParam p)

deriving instance ShowExtension p => Show (FormalParam p)

deriving instance ReadExtension p => Read (FormalParam p)

deriving instance DataExtension p => Data (FormalParam p)

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
newtype MethodBody p = MethodBody (Maybe (Block p))
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (MethodBody p)

deriving instance ShowExtension p => Show (MethodBody p)

deriving instance ReadExtension p => Read (MethodBody p)

deriving instance DataExtension p => Data (MethodBody p)

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody p = ConstructorBody (Maybe (ExplConstrInv p)) [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (ConstructorBody p)

deriving instance ShowExtension p => Show (ConstructorBody p)

deriving instance ReadExtension p => Read (ConstructorBody p)

deriving instance DataExtension p => Data (ConstructorBody p)

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv p
  = ThisInvoke [RefType] [Argument p]
  | SuperInvoke [RefType] [Argument p]
  | PrimarySuperInvoke (Exp p) [RefType] [Argument p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (ExplConstrInv p)

deriving instance ShowExtension p => Show (ExplConstrInv p)

deriving instance ReadExtension p => Read (ExplConstrInv p)

deriving instance DataExtension p => Data (ExplConstrInv p)

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

deriving instance EqExtension p => Eq (Modifier p)

deriving instance ShowExtension p => Show (Modifier p)

deriving instance ReadExtension p => Read (Modifier p)

deriving instance DataExtension p => Data (Modifier p)

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

deriving instance EqExtension p => Eq (Annotation p)

deriving instance ShowExtension p => Show (Annotation p)

deriving instance ReadExtension p => Read (Annotation p)

deriving instance DataExtension p => Data (Annotation p)

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

deriving instance EqExtension p => Eq (ElementValue p)

deriving instance ShowExtension p => Show (ElementValue p)

deriving instance ReadExtension p => Read (ElementValue p)

deriving instance DataExtension p => Data (ElementValue p)

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
newtype Block p = Block [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (Block p)

deriving instance ShowExtension p => Show (Block p)

deriving instance ReadExtension p => Read (Block p)

deriving instance DataExtension p => Data (Block p)

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt p
  = BlockStmt SourceSpan (Stmt p)
  | LocalClass (ClassDecl p)
  | LocalVars SourceSpan [Modifier p] Type [VarDecl p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (BlockStmt p)

deriving instance ShowExtension p => Show (BlockStmt p)

deriving instance ReadExtension p => Read (BlockStmt p)

deriving instance DataExtension p => Data (BlockStmt p)

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

deriving instance EqExtension p => Eq (Stmt p)

deriving instance ShowExtension p => Show (Stmt p)

deriving instance ReadExtension p => Read (Stmt p)

deriving instance DataExtension p => Data (Stmt p)

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch p = Catch (FormalParam p) (Block p)
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (Catch p)

deriving instance ShowExtension p => Show (Catch p)

deriving instance ReadExtension p => Read (Catch p)

deriving instance DataExtension p => Data (Catch p)

data TryResource p
  = TryResourceVarDecl (ResourceDecl p)
  | TryResourceVarAccess Ident
  | TryResourceQualAccess (FieldAccess p)
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (TryResource p)

deriving instance ShowExtension p => Show (TryResource p)

deriving instance ReadExtension p => Read (TryResource p)

deriving instance DataExtension p => Data (TryResource p)

data ResourceDecl p
  = ResourceDecl [Modifier p] Type VarDeclId (VarInit p)
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (ResourceDecl p)

deriving instance ShowExtension p => Show (ResourceDecl p)

deriving instance ReadExtension p => Read (ResourceDecl p)

deriving instance DataExtension p => Data (ResourceDecl p)

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock p
  = SwitchBlock SourceSpan (SwitchLabel p) [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (SwitchBlock p)

deriving instance ShowExtension p => Show (SwitchBlock p)

deriving instance ReadExtension p => Read (SwitchBlock p)

deriving instance DataExtension p => Data (SwitchBlock p)

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

deriving instance EqExtension p => Eq (SwitchLabel p)

deriving instance ShowExtension p => Show (SwitchLabel p)

deriving instance ReadExtension p => Read (SwitchLabel p)

deriving instance DataExtension p => Data (SwitchLabel p)

data SwitchExpBranch p
  = SwitchExpBranch (SwitchLabel p) (SwitchExpBranchBody p)
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (SwitchExpBranch p)

deriving instance ShowExtension p => Show (SwitchExpBranch p)

deriving instance ReadExtension p => Read (SwitchExpBranch p)

deriving instance DataExtension p => Data (SwitchExpBranch p)

data SwitchExpBranchBody p
  = SwitchExpBranchExp (Exp p)
  | SwitchExpBranchBlock [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (SwitchExpBranchBody p)

deriving instance ShowExtension p => Show (SwitchExpBranchBody p)

deriving instance ReadExtension p => Read (SwitchExpBranchBody p)

deriving instance DataExtension p => Data (SwitchExpBranchBody p)

-- | Initialization code for a basic @for@ statement.
data ForInit p
  = ForLocalVars [Modifier p] Type [VarDecl p]
  | ForInitExps [Exp p]
  deriving (Generic, Typeable)

deriving instance EqExtension p => Eq (ForInit p)

deriving instance ShowExtension p => Show (ForInit p)

deriving instance ReadExtension p => Read (ForInit p)

deriving instance DataExtension p => Data (ForInit p)

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

deriving instance EqExtension p => Eq (Exp p)

deriving instance ShowExtension p => Show (Exp p)

deriving instance ReadExtension p => Read (Exp p)

deriving instance DataExtension p => Data (Exp p)

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

deriving instance EqExtension p => Eq (Lhs p)

deriving instance ShowExtension p => Show (Lhs p)

deriving instance ReadExtension p => Read (Lhs p)

deriving instance DataExtension p => Data (Lhs p)

-- | Array access
data ArrayIndex p
  = -- | Index into an array
    ArrayIndex (Exp p) [Exp p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (ArrayIndex p)

deriving instance ShowExtension p => Show (ArrayIndex p)

deriving instance ReadExtension p => Read (ArrayIndex p)

deriving instance DataExtension p => Data (ArrayIndex p)

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

deriving instance EqExtension p => Eq (FieldAccess p)

deriving instance ShowExtension p => Show (FieldAccess p)

deriving instance ReadExtension p => Read (FieldAccess p)

deriving instance DataExtension p => Data (FieldAccess p)

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParams p
  = LambdaSingleParam Ident
  | LambdaFormalParams [FormalParam p]
  | LambdaInferredParams [Ident]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (LambdaParams p)

deriving instance ShowExtension p => Show (LambdaParams p)

deriving instance ReadExtension p => Read (LambdaParams p)

deriving instance DataExtension p => Data (LambdaParams p)

-- | Lambda expression, starting from java 8
data LambdaExpression p
  = LambdaExpression (Exp p)
  | LambdaBlock (Block p)
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (LambdaExpression p)

deriving instance ShowExtension p => Show (LambdaExpression p)

deriving instance ReadExtension p => Read (LambdaExpression p)

deriving instance DataExtension p => Data (LambdaExpression p)

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

deriving instance EqExtension p => Eq (MethodInvocation p)

deriving instance ShowExtension p => Show (MethodInvocation p)

deriving instance ReadExtension p => Read (MethodInvocation p)

deriving instance DataExtension p => Data (MethodInvocation p)

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
newtype ArrayInit p
  = ArrayInit [VarInit p]
  deriving (Typeable, Generic)

deriving instance EqExtension p => Eq (ArrayInit p)

deriving instance ShowExtension p => Show (ArrayInit p)

deriving instance ReadExtension p => Read (ArrayInit p)

deriving instance DataExtension p => Data (ArrayInit p)

data MethodRefTarget
  = MethodRefIdent Ident
  | MethodRefConstructor
  deriving (Eq, Show, Read, Typeable, Generic, Data)
