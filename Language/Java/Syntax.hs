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
import Language.Java.Syntax.Exp
import Language.Java.Syntax.Types

-----------------------------------------------------------------------
-- Packages

-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit = CompilationUnit (Maybe PackageDecl) [ImportDecl] [TypeDecl]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located CompilationUnit

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
newtype PackageDecl = PackageDecl Name
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located PackageDecl

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl
  = ImportDecl SourceSpan Bool {- static? -} Name Bool {- .*? -}
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located ImportDecl where
  sourceSpan (ImportDecl s _ _ _) = s

-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl
  = ClassTypeDecl ClassDecl
  | InterfaceTypeDecl InterfaceDecl
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located TypeDecl

-- | A class declaration specifies a new named reference type.
data ClassDecl
  = ClassDecl SourceSpan [Modifier] Ident [TypeParam] (Maybe RefType) [RefType] ClassBody
  | RecordDecl SourceSpan [Modifier] Ident [TypeParam] [RecordFieldDecl] [RefType] ClassBody
  | EnumDecl SourceSpan [Modifier] Ident [RefType] EnumBody
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located ClassDecl where
  sourceSpan (ClassDecl s _ _ _ _ _ _) = s
  sourceSpan (RecordDecl s _ _ _ _ _ _) = s
  sourceSpan (EnumDecl s _ _ _ _) = s

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
newtype ClassBody = ClassBody [Decl]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located ClassBody

-- | The body of an enum type may contain enum constants.
data EnumBody = EnumBody [EnumConstant] [Decl]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located EnumBody

-- | An enum constant defines an instance of the enum type.
data EnumConstant = EnumConstant Ident [Argument] (Maybe ClassBody)
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located EnumConstant

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl
  = InterfaceDecl SourceSpan InterfaceKind [Modifier] Ident [TypeParam] [RefType {- extends -}] [RefType {- permits -}] InterfaceBody
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located InterfaceDecl where
  sourceSpan (InterfaceDecl s _ _ _ _ _ _ _) = s

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located InterfaceKind

-- | The body of an interface may declare members of the interface.
newtype InterfaceBody
  = InterfaceBody [MemberDecl]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located InterfaceBody

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl
  = MemberDecl MemberDecl
  | InitDecl Bool Block
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located Decl

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
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located MemberDecl where
  sourceSpan (FieldDecl s _ _ _) = s
  sourceSpan (MethodDecl s _ _ _ _ _ _ _ _) = s
  sourceSpan (ConstructorDecl s _ _ _ _ _ _) = s
  sourceSpan _ = dummySourceSpan

-- | A field declaration of a record
data RecordFieldDecl
  = RecordFieldDecl Type Ident
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located RecordFieldDecl

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl
  = VarDecl SourceSpan VarDeclId (Maybe VarInit)
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located VarDecl where
  sourceSpan (VarDecl s _ _) = s

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId
  = VarId Ident
  | -- | Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
    VarDeclArray SourceSpan VarDeclId
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located VarDeclId where
  sourceSpan (VarDeclArray s _) = s
  sourceSpan _ = dummySourceSpan

-- | Explicit initializer for a variable declaration.
data VarInit
  = InitExp Exp
  | InitArray ArrayInit
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located VarInit

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam = FormalParam SourceSpan [Modifier] Type Bool VarDeclId
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located FormalParam where
  sourceSpan (FormalParam s _ _ _ _) = s

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
newtype MethodBody = MethodBody (Maybe Block)
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located MethodBody

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody = ConstructorBody (Maybe ExplConstrInv) [BlockStmt]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located ConstructorBody

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv
  = ThisInvoke [RefType] [Argument]
  | SuperInvoke [RefType] [Argument]
  | PrimarySuperInvoke Exp [RefType] [Argument]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located ExplConstrInv

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
  deriving (Eq, Read, Typeable, Generic, Data)

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

instance Located Modifier where
  sourceSpan (Public s) = s
  sourceSpan (Abstract s) = s
  sourceSpan _ = dummySourceSpan

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
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located Annotation where
  sourceSpan (NormalAnnotation s _ _) = s
  sourceSpan (SingleElementAnnotation s _ _) = s
  sourceSpan (MarkerAnnotation s _) = s

desugarAnnotation (MarkerAnnotation s n) = (s, n, [])
-- TODO: check span for ident
desugarAnnotation (SingleElementAnnotation s n e) = (s, n, [(Ident dummySourceSpan "value", e)])
desugarAnnotation (NormalAnnotation s n kv) = (s, n, kv)

desugarAnnotation' (MarkerAnnotation s n) = NormalAnnotation s n []
-- TODO: check span for ident
desugarAnnotation' (SingleElementAnnotation s n e) = NormalAnnotation s n [(Ident dummySourceSpan "value", e)]
desugarAnnotation' normal = normal

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue
  = EVVal VarInit
  | EVAnn Annotation
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located ElementValue

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
newtype Block = Block [BlockStmt]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located Block

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt
  = BlockStmt SourceSpan Stmt
  | LocalClass ClassDecl
  | LocalVars SourceSpan [Modifier] Type [VarDecl]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located BlockStmt where
  sourceSpan (BlockStmt s _) = s
  sourceSpan (LocalVars s _ _ _) = s
  sourceSpan _ = dummySourceSpan

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
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located Stmt where
  sourceSpan (IfThen s _ _) = s
  sourceSpan (IfThenElse s _ _ _) = s
  sourceSpan (While s _ _) = s
  sourceSpan (BasicFor s _ _ _ _) = s
  sourceSpan (EnhancedFor s _ _ _ _ _) = s
  sourceSpan (ExpStmt s _) = s
  sourceSpan (Do s _ _) = s
  sourceSpan (Break s _) = s
  sourceSpan (Return s _) = s
  sourceSpan (Try s _ _ _ _) = s
  sourceSpan _ = dummySourceSpan

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch = Catch FormalParam Block
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located Catch

data TryResource
  = TryResourceVarDecl ResourceDecl
  | TryResourceVarAccess Ident
  | TryResourceQualAccess FieldAccess
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located TryResource

data ResourceDecl
  = ResourceDecl [Modifier] Type VarDeclId VarInit
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located ResourceDecl

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock
  = SwitchBlock SourceSpan SwitchLabel [BlockStmt]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located SwitchBlock where
  sourceSpan (SwitchBlock s _ _) = s

data SwitchStyle
  = SwitchOldStyle
  | SwitchNewStyle -- JEP 361
  deriving (Eq, Show, Read, Typeable, Generic, Data)

-- | A label within a @switch@ statement.
data SwitchLabel
  = -- | The expressions contained in the @case@ must be a 'Lit' or an @enum@ constant.
    -- The list must be non-empty.
    SwitchCase [Exp]
  | Default
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located SwitchLabel

data SwitchExpBranch
  = SwitchExpBranch SwitchLabel SwitchExpBranchBody
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located SwitchExpBranch

data SwitchExpBranchBody
  = SwitchExpBranchExp Exp
  | SwitchExpBranchBlock [BlockStmt]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located SwitchExpBranchBody

-- | Initialization code for a basic @for@ statement.
data ForInit
  = ForLocalVars [Modifier] Type [VarDecl]
  | ForInitExps [Exp]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located ForInit

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
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located Exp where
  sourceSpan (PostIncrement s _) = s
  sourceSpan (PostDecrement s _) = s
  sourceSpan (PreIncrement s _) = s
  sourceSpan (PreDecrement s _) = s
  sourceSpan (Cond s _ _ _) = s
  sourceSpan (Assign s _ _ _) = s
  sourceSpan _ = dummySourceSpan

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
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located Lhs

-- | Array access
data ArrayIndex
  = -- | Index into an array
    ArrayIndex Exp [Exp]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located ArrayIndex

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess
  = -- | Accessing a field of an object or array computed from an expression.
    PrimaryFieldAccess Exp Ident
  | -- | Accessing a field of the superclass.
    SuperFieldAccess Ident
  | -- | Accessing a (static) field of a named class.
    ClassFieldAccess Name Ident
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located FieldAccess

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParams
  = LambdaSingleParam Ident
  | LambdaFormalParams [FormalParam]
  | LambdaInferredParams [Ident]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located LambdaParams

-- | Lambda expression, starting from java 8
data LambdaExpression
  = LambdaExpression Exp
  | LambdaBlock Block
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located LambdaExpression

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
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located MethodInvocation

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
newtype ArrayInit
  = ArrayInit [VarInit]
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located ArrayInit

data MethodRefTarget
  = MethodRefIdent Ident
  | MethodRefConstructor
  deriving (Eq, Show, Read, Typeable, Generic, Data)

instance Located MethodRefTarget
