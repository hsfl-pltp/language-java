{-# LANGUAGE CPP #-}

module Language.Java.Parser
  ( JavaParser,
    parser,
    parserWithMode,
    ParserMode (..),
    parserWithState,
    ParserState (..),
    getLocation,
    compilationUnit,
    packageDecl,
    importDecl,
    typeDecl,
    classDecl,
    interfaceDecl,
    memberDecl,
    fieldDecl,
    methodDecl,
    constrDecl,
    interfaceMemberDecl,
    absMethodDecl,
    formalParams,
    formalParam,
    modifier,
    varDecls,
    varDecl,
    block,
    blockStmt,
    stmt,
    stmtExp,
    exp,
    primary,
    literal,
    ttype,
    primType,
    refType,
    classType,
    resultType,
    lambdaExp,
    methodRef,
    typeParams,
    typeParam,
    name,
    ident,
    empty,
    list,
    list1,
    seplist,
    seplist1,
    opt,
    bopt,
    lopt,
    comma,
    semiColon,
    period,
    colon,
  )
where

import Data.Maybe (catMaybes, isJust)
import Language.Java.Lexer (L (..), Token (..), lexer)
import Language.Java.Pretty (pretty)
import Language.Java.Syntax
import Text.Parsec hiding (Empty)
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Prelude hiding (exp, (>>), (>>=))
import qualified Prelude as P ((>>), (>>=))

#if __GLASGOW_HASKELL__ < 707
import Control.Applicative ( (<$>), (<$), (<*) )
-- Since I cba to find the instance Monad m => Applicative m declaration.
(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
infixl 4 <*>
#else
import Control.Applicative ( (<$>), (<$), (<*), (<*>) )
import GHC.IO (unsafePerformIO)
#endif

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

data ParserState = ParserState
  { ps_mode :: ParserMode,
    ps_locations :: Bool
  }
  deriving (Eq, Show)

data ParserMode
  = ParseFull -- the default
  | ParseShallow -- do not parse methods bodies
  deriving (Eq, Show)

defaultParserState :: ParserState
defaultParserState = ParserState ParseFull True

type JavaParser = Parsec [L Token] ParserState

type P = JavaParser

noLoc :: P (a, Location) -> P a
noLoc = fmap fst

getLocation :: P Location
getLocation = do
  myState <- getState
  if ps_locations myState
    then do
      state <- getParserState
      case stateInput state of
        [] -> return locationEof
        _ ->
          let p = statePos state
           in return $ Location {loc_file = sourceName p, loc_line = sourceLine p, loc_column = sourceColumn p}
    else return dummyLocation

getNextTok :: P (Maybe (L Token))
getNextTok = do
  state <- getParserState
  case stateInput state of
    [] -> return Nothing
    (x : _) -> return (Just x)

traceP :: String -> P ()
traceP s =
  unsafePerformIO (logToFile s) `seq` return ()

logToFile :: String -> IO ()
logToFile msg =
  appendFile "hs-java-parser.log" (msg ++ "\n")

-- A trick to allow >> and >>=, normally infixr 1, to be
-- used inside branches of <|>, which is declared as infixl 1.
-- There are no clashes with other operators of precedence 2.
(>>) :: P a -> P b -> P b
(>>) = (P.>>)

(>>=) :: P a -> (a -> P b) -> P b
(>>=) = (P.>>=)

infixr 2 >>, >>=

-- Note also when reading that <$> is infixl 4 and thus has
-- lower precedence than all the others (>>, >>=, and <|>).

----------------------------------------------------------------------------
-- Top-level parsing

parser :: P a -> FilePath -> String -> Either ParseError a
parser = parserWithState defaultParserState

parserWithMode :: ParserMode -> P a -> FilePath -> String -> Either ParseError a
parserWithMode mode = parserWithState (ParserState mode True)

parserWithState :: ParserState -> P a -> FilePath -> String -> Either ParseError a
parserWithState state p srcName src =
  case lexer src of
    Left (line, col, err) ->
      let msg = Message err
          pos = newPos srcName line col
       in Left (newErrorMessage msg pos)
    Right tokens ->
      runParser p state srcName tokens

----------------------------------------------------------------------------
-- Packages and compilation units

compilationUnit :: P CompilationUnit
compilationUnit = do
  mpd <- opt packageDecl
  ids <- list importDecl
  tds <- list typeDecl
  eof
  return $ CompilationUnit mpd ids (catMaybes tds)

packageDecl :: P PackageDecl
packageDecl = do
  tok KW_Package
  n <- name
  semiColon
  return $ PackageDecl n

importDecl :: P ImportDecl
importDecl = do
  startLoc <- getLocation
  tok KW_Import
  st <- bopt $ tok KW_Static
  n <- name
  ds <- bopt $ period >> tok Op_Star
  semiColon
  endLoc <- getLocation
  return (ImportDecl (startLoc, endLoc) st n ds)

typeDecl :: P (Maybe TypeDecl)
typeDecl =
  Just <$> classOrInterfaceDecl
    <|> const Nothing <$> semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDecl :: P TypeDecl
classOrInterfaceDecl = do
  startLoc <- getLocation
  ms <- list modifier
  de <-
    ( do
        cd <- classDecl
        return $ ClassTypeDecl (cd startLoc ms)
      )
      <|> ( do
              id <- annInterfaceDecl <|> interfaceDecl
              return $ InterfaceTypeDecl (id startLoc ms)
          )
  return $ de

classDecl :: P (Mod ClassDecl)
classDecl = normalClassDecl <|> recordClassDecl <|> enumClassDecl

normalClassDecl :: P (Mod ClassDecl)
normalClassDecl = do
  tok KW_Class
  i <- ident
  tps <- lopt typeParams
  mex <- opt extends
  imp <- lopt implements
  (bod, endLoc) <- classBody
  return $ \loc ms -> ClassDecl (loc, endLoc) ms i tps (fmap head mex) imp bod

extends :: P [RefType]
extends = tok KW_Extends >> refTypeList

permits :: P [RefType]
permits = fixedIdent "permits" () >> refTypeList

implements :: P [RefType]
implements = tok KW_Implements >> refTypeList

enumClassDecl :: P (Mod ClassDecl)
enumClassDecl = do
  tok KW_Enum
  i <- ident
  imp <- lopt implements
  (bod, endLoc) <- enumBody
  return $ \loc ms -> EnumDecl (loc, endLoc) ms i imp bod

recordClassDecl :: P (Mod ClassDecl)
recordClassDecl = do
  tok KW_Record
  i <- ident
  tps <- lopt typeParams
  fields <- parens (seplist recordField comma)
  imp <- lopt implements
  (bod, endLoc) <- classBody
  return $ \loc ms -> RecordDecl (loc, endLoc) ms i tps fields imp bod
  where
    recordField = do
      typ <- ttype
      i <- ident
      return $ RecordFieldDecl typ i

classBody :: P (ClassBody, Location)
classBody = do
  (b, loc) <- braces classBodyStatements
  return (ClassBody b, loc)

enumBody :: P (EnumBody, Location)
enumBody = braces $ do
  ecs <- seplist enumConst comma
  optional comma
  eds <- lopt enumBodyDecls
  return $ EnumBody ecs eds

enumConst :: P EnumConstant
enumConst = do
  id <- ident
  as <- lopt args
  mcb <- opt (noLoc classBody)
  return $ EnumConstant id as mcb

enumBodyDecls :: P [Decl]
enumBodyDecls = semiColon >> classBodyStatements

classBodyStatements :: P [Decl]
classBodyStatements = catMaybes <$> list classBodyStatement

-- Interface declarations

annInterfaceDecl :: P (Mod InterfaceDecl)
annInterfaceDecl = do
  tok KW_AnnInterface
  id <- ident
  tps <- lopt typeParams
  exs <- lopt extends
  ps <- lopt permits
  (bod, endLoc) <- interfaceBody
  return $ \loc ms -> InterfaceDecl (loc, endLoc) InterfaceAnnotation ms id tps exs ps bod

interfaceDecl :: P (Mod InterfaceDecl)
interfaceDecl = do
  tok KW_Interface
  id <- ident
  tps <- lopt typeParams
  exs <- lopt extends
  ps <- lopt permits
  (bod, endLoc) <- interfaceBody
  return $ \loc ms -> InterfaceDecl (loc, endLoc) InterfaceNormal ms id tps exs ps bod

interfaceBody :: P (InterfaceBody, Location)
interfaceBody = braces (InterfaceBody . catMaybes <$> (list interfaceBodyDecl))

-- Declarations

classBodyStatement :: P (Maybe Decl)
classBodyStatement =
  ( try $ do
      list1 semiColon
      return Nothing
  )
    <|> ( try $ do
            mst <- bopt (tok KW_Static)
            blk <- blockNoLoc
            return $ Just $ InitDecl mst blk
        )
    <|> ( do
            loc <- getLocation
            ms <- list modifier
            dec <- memberDecl
            return $ Just $ MemberDecl (dec loc ms)
        )

memberDecl :: P (Mod MemberDecl)
memberDecl =
  ( try $ do
      cd <- classDecl
      return $ \loc ms -> MemberClassDecl (cd loc ms)
  )
    <|> ( try $ do
            id <- try annInterfaceDecl <|> try interfaceDecl
            return $ \loc ms -> MemberInterfaceDecl (id loc ms)
        )
    <|> try fieldDecl
    <|> try methodDecl
    <|> constrDecl

fieldDecl :: P (Mod MemberDecl)
fieldDecl = do
  typ <- ttype
  vds <- varDecls
  semiColon
  endLoc <- getLocation
  return $ \loc ms -> FieldDecl (loc, endLoc) ms typ vds

methodDecl :: P (Mod MemberDecl)
methodDecl = do
  tps <- lopt typeParams
  rt <- resultType
  id <- ident
  fps <- formalParams
  thr <- lopt throws
  (bod, endLoc) <- methodBody
  return $ \loc ms -> MethodDecl (loc, endLoc) ms tps rt id fps thr Nothing bod

methodBody :: P (MethodBody, Location)
methodBody = onlySemi <|> fullBody
  where
    onlySemi = do
      loc <- getLocation
      semiColon
      return (MethodBody Nothing, loc)
    fullBody = do
      (b, loc) <- block
      return (MethodBody (Just b), loc)

constrDecl :: P (Mod MemberDecl)
constrDecl = do
  tps <- lopt typeParams
  id <- ident
  fps <- optList formalParams -- record constructors omit the argument list
  thr <- lopt throws
  bod <- constrBody
  endLoc <- getLocation
  return $ \loc ms -> ConstructorDecl (loc, endLoc) ms tps id fps thr bod

constrBody :: P ConstructorBody
constrBody = noLoc . braces $ do
  mec <- opt (try explConstrInv)
  bss <- list blockStmt
  return $ ConstructorBody mec bss

explConstrInv :: P ExplConstrInv
explConstrInv =
  noLoc . endSemi $
    ( try $ do
        tas <- lopt refTypeArgs
        tok KW_This
        as <- args
        return $ ThisInvoke tas as
    )
      <|> ( try $ do
              tas <- lopt refTypeArgs
              tok KW_Super
              as <- args
              return $ SuperInvoke tas as
          )
      <|> ( do
              pri <- primary
              period
              tas <- lopt refTypeArgs
              tok KW_Super
              as <- args
              return $ PrimarySuperInvoke pri tas as
          )

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: P (Maybe MemberDecl)
interfaceBodyDecl =
  semiColon
    >> return Nothing
      <|> do
        loc <- getLocation
        ms <- list modifier
        imd <- interfaceMemberDecl
        return $ Just (imd loc ms)

interfaceMemberDecl :: P (Mod MemberDecl)
interfaceMemberDecl =
  ( do
      cd <- classDecl
      return $ \loc ms -> MemberClassDecl (cd loc ms)
  )
    <|> ( do
            id <- try annInterfaceDecl <|> try interfaceDecl
            return $ \loc ms -> MemberInterfaceDecl (id loc ms)
        )
    <|> try fieldDecl
    <|> absMethodDecl

absMethodDecl :: P (Mod MemberDecl)
absMethodDecl = do
  tps <- lopt typeParams
  rt <- resultType
  id <- ident
  fps <- formalParams
  thr <- lopt throws
  def <- opt defaultValue
  semiColon
  endLoc <- getLocation
  return $ \loc ms -> MethodDecl (loc, endLoc) ms tps rt id fps thr def (MethodBody Nothing)

defaultValue :: P Exp
defaultValue = tok KW_Default >> exp

throws :: P [RefType]
throws = tok KW_Throws >> refTypeList

-- Formal parameters

formalParams :: P [FormalParam]
formalParams = parens $ do
  fps <- seplist formalParam comma
  if validateFPs fps
    then return fps
    else fail "Only the last formal parameter may be of variable arity"
  where
    validateFPs :: [FormalParam] -> Bool
    validateFPs [] = True
    validateFPs [_] = True
    validateFPs (FormalParam _ _ b _ : xs) = not b

formalParam :: P FormalParam
formalParam = do
  ms <- list modifier
  typ <- ttype
  var <- bopt ellipsis
  vid <- varDeclId
  return $ FormalParam ms typ var vid

ellipsis :: P ()
ellipsis = period >> period >> period

-- Modifiers

modifier :: P Modifier
modifier =
  tok KW_Protected
    >> return Protected
      <|> tok KW_Private
    >> return Private
      <|> tok KW_Static
    >> return Static
      <|> tok KW_Strictfp
    >> return StrictFP
      <|> tok KW_Final
    >> return Final
      <|> tok KW_Native
    >> return Native
      <|> tok KW_Transient
    >> return Transient
      <|> tok KW_Volatile
    >> return Volatile
      <|> tok KW_Synchronized
    >> return Synchronized_
      <|> fixedIdent "sealed" Sealed
      <|> Annotation <$> annotation
      <|> ( do
              startLoc <- getLocation
              ( do
                  tok KW_Public
                  endLoc <- getLocation
                  return (Public (startLoc, endLoc))
                )
                <|> ( do
                        tok KW_Abstract
                        endLoc <- getLocation
                        return (Abstract (startLoc, endLoc))
                    )
          )

annotation :: P Annotation
annotation = do
  startLoc <- getLocation
  flip ($)
    <$ tok Op_AtSign
    <*> name
    <*> ( try
            ( do
                elist <- parens evlist
                endLoc <- getLocation
                return (flip (NormalAnnotation (startLoc, endLoc)) elist)
            )
            <|> try
              ( do
                  e <- parens elementValue
                  endLoc <- getLocation
                  return (flip (SingleElementAnnotation (startLoc, endLoc)) e)
              )
            <|> try
              ( do
                  endLoc <- getLocation
                  MarkerAnnotation (startLoc, endLoc) <$ return ()
              )
        )

evlist :: P [(Ident, ElementValue)]
evlist = seplist1 elementValuePair comma

elementValuePair :: P (Ident, ElementValue)
elementValuePair = (,) <$> ident <* tok Op_Equal <*> elementValue

elementValue :: P ElementValue
elementValue =
  EVVal
    <$> ( InitArray <$> arrayInit
            <|> InitExp <$> condExp
        )
    <|> EVAnn
      <$> annotation

----------------------------------------------------------------------------
-- Variable declarations

varDecls :: P [VarDecl]
varDecls = seplist1 varDecl comma

varDecl :: P VarDecl
varDecl = do
  vid <- varDeclId
  mvi <- opt $ tok Op_Equal >> varInit
  return $ VarDecl vid mvi

varDeclId :: P VarDeclId
varDeclId = do
  startLoc <- getLocation
  id <- ident
  abs <- list arrBrackets
  endLoc <- getLocation
  return $ foldl (\f _ -> VarDeclArray (startLoc, endLoc) . f) VarId abs id

arrBrackets :: P ()
arrBrackets = brackets $ return ()

localVarDecl :: P (Location, [Modifier], Type, [VarDecl])
localVarDecl = do
  startLoc <- getLocation
  ms <- list modifier
  typ <- ttype
  vds <- varDecls
  return (startLoc, ms, typ, vds)

varInit :: P VarInit
varInit =
  InitArray <$> arrayInit
    <|> InitExp <$> exp

arrayInit :: P ArrayInit
arrayInit = noLoc . braces $ do
  vis <- seplist varInit comma
  opt comma
  return $ ArrayInit vis

----------------------------------------------------------------------------
-- Statements

block :: P (Block, Location)
block = do
  state <- getState
  case ps_mode state of
    ParseFull -> braces $ Block <$> list blockStmt
    ParseShallow -> shallowP
  where
    shallowP = do
      loc <- parseNestedCurly (-1)
      return (Block [], loc)

blockNoLoc :: P Block
blockNoLoc = fmap fst block

-- | Parses anything between properly balance curly brackets.
-- level must initially be -1
parseNestedCurly :: Int -> P Location
parseNestedCurly level = do
  loc <- getLocation
  newLevel <-
    javaToken $ \t ->
      case t of
        OpenCurly -> Just (level + 1)
        _ | level < 0 -> Nothing -- need to start with {
        CloseCurly -> Just (level - 1)
        _ -> Just level
  if newLevel < 0 then return loc else parseNestedCurly newLevel

blockStmt :: P BlockStmt
blockStmt =
  try
    ( do
        loc <- getLocation
        ms <- list modifier
        cd <- classDecl
        return (LocalClass (cd loc ms))
    )
    <|> try
      ( do
          ((startLoc, m, t, vds), endLoc) <- endSemi localVarDecl
          return (LocalVars (startLoc, endLoc) m t vds)
      )
    <|> do
      startLoc <- getLocation
      (s, endLoc) <- stmt
      return (BlockStmt (startLoc, endLoc) s)

stmt :: P (Stmt, Location)
stmt = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
      startLoc <- getLocation
      tok KW_If
      e <- parens exp
      try
        ( do
            th <- noLoc stmtNSI
            tok KW_Else
            (el, endLoc) <- stmt
            return (IfThenElse (startLoc, endLoc) e th el, endLoc)
        )
        <|> ( do
                (th, endLoc) <- stmt
                return (IfThen (startLoc, endLoc) e th, endLoc)
            )
    whileStmt = do
      tok KW_While
      e <- parens exp
      (s, endLoc) <- stmt
      return (While e s, endLoc)
    forStmt = do
      tok KW_For
      f <-
        parens $
          ( try $ do
              fi <- opt forInit
              semiColon
              e <- opt exp
              semiColon
              fu <- opt forUp
              return $ BasicFor fi e fu
          )
            <|> ( do
                    ms <- list modifier
                    t <- ttype
                    i <- ident
                    colon
                    e <- exp
                    return $ EnhancedFor ms t i e
                )
      (s, endLoc) <- stmt
      return (f s, endLoc)
    labeledStmt = try $ do
      lbl <- ident
      colon
      (s, endLoc) <- stmt
      return (Labeled lbl s, endLoc)

stmtNSI :: P (Stmt, Location)
stmtNSI = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
      startLoc <- getLocation
      tok KW_If
      e <- parens exp
      th <- noLoc stmtNSI
      tok KW_Else
      (el, endLoc) <- stmtNSI
      return (IfThenElse (startLoc, endLoc) e th el, endLoc)
    whileStmt = do
      tok KW_While
      e <- parens exp
      (s, endLoc) <- stmtNSI
      return (While e s, endLoc)
    forStmt = do
      tok KW_For
      f <-
        parens $
          ( try $ do
              fi <- opt forInit
              semiColon
              e <- opt exp
              semiColon
              fu <- opt forUp
              return $ BasicFor fi e fu
          )
            <|> ( do
                    ms <- list modifier
                    t <- ttype
                    i <- ident
                    colon
                    e <- exp
                    return $ EnhancedFor ms t i e
                )
      (s, endLoc) <- stmtNSI
      return (f s, endLoc)
    labeledStmt = try $ do
      i <- ident
      colon
      (s, endLoc) <- stmtNSI
      return (Labeled i s, endLoc)

stmtNoTrail :: P (Stmt, Location)
stmtNoTrail =
  -- empty statement
  do
    semiColon
    endLoc <- getLocation
    return (Empty, endLoc)
    <|>
    -- inner block
    mapFst StmtBlock <$> block
    <|>
    -- assertions
    ( endSemi $ do
        tok KW_Assert
        e <- exp
        me2 <- opt $ colon >> exp
        return $ Assert e me2
    )
    <|>
    -- switch stmts
    ( do
        tok KW_Switch
        e <- parens exp
        ((style, sb), endLoc) <- switchBlock
        return (Switch style e sb, endLoc)
    )
    <|>
    -- do-while loops
    ( endSemi $ do
        tok KW_Do
        s <- noLoc stmt
        tok KW_While
        e <- parens exp
        return $ Do s e
    )
    <|>
    -- break
    ( endSemi $ do
        tok KW_Break
        mi <- opt ident
        return $ Break mi
    )
    <|>
    -- continue
    ( endSemi $ do
        tok KW_Continue
        mi <- opt ident
        return $ Continue mi
    )
    <|>
    -- return
    ( endSemi $ do
        tok KW_Return
        me <- opt exp
        return $ Return me
    )
    <|>
    -- synchronized
    ( do
        tok KW_Synchronized
        e <- parens exp
        (b, endLoc) <- block
        return (Synchronized e b, endLoc)
    )
    <|>
    -- throw
    ( endSemi $ do
        tok KW_Throw
        e <- exp
        return $ Throw e
    )
    <|>
    -- try-catch, both with and without a finally clause
    ( do
        startLoc <- getLocation
        tok KW_Try
        resources <- tryResourceList
        b <- noLoc block
        blockEndLoc <- getLocation
        c <- list catch
        catchEndLoc <- getLocation
        mf <- opt (tok KW_Finally >> noLoc block)
        finallyEndLoc <- getLocation
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        let endLoc = case (mf, c) of
              (Nothing, []) -> blockEndLoc
              (Nothing, _) -> catchEndLoc
              (Just _, _) -> finallyEndLoc
        return
          ( Try (startLoc, endLoc) resources b c mf,
            endLoc
          )
    )
    <|>
    -- expressions as stmts
    do
      startLoc <- getLocation
      (s, endLoc) <- endSemi stmtExp
      return (ExpStmt (startLoc, endLoc) s, endLoc)

-- For loops

forInit :: P ForInit
forInit =
  ( do
      try
        ( do
            (_, m, t, vds) <- localVarDecl
            return $ ForLocalVars m t vds
        )
  )
    <|> (seplist1 stmtExp comma >>= return . ForInitExps)

forUp :: P [Exp]
forUp = seplist1 stmtExp comma

-- Switches

switchBlock :: P ((SwitchStyle, [SwitchBlock]), Location)
switchBlock = braces (try old <|> new)
  where
    old = do
      x <- list switchStmtOld
      return (SwitchOldStyle, x)
    new = do
      x <- list switchStmtNew
      return (SwitchNewStyle, x)

switchStmtOld :: P SwitchBlock
switchStmtOld = do
  lbl <- switchLabelOld
  bss <- list blockStmt
  return $ SwitchBlock lbl bss

switchLabelOld :: P SwitchLabel
switchLabelOld =
  (tok KW_Default >> colon >> return Default)
    <|> ( do
            tok KW_Case
            es <- seplist condExp comma
            colon
            return $ SwitchCase es
        )

switchStmtNew :: P SwitchBlock
switchStmtNew = do
  lbl <- switchLabelNew
  bss <- noLoc (braces (list blockStmt)) <|> (blockStmt >>= \s -> return [s])
  return $ SwitchBlock lbl bss

switchLabelNew :: P SwitchLabel
switchLabelNew =
  (tok KW_Default >> tok LambdaArrow >> return Default)
    <|> ( do
            tok KW_Case
            es <- seplist condExp comma
            tok LambdaArrow
            return $ SwitchCase es
        )

switchExp :: P Exp
switchExp = do
  tok KW_Switch
  e <- parens exp
  branches <- noLoc (braces switchExpBody)
  return $ SwitchExp e branches
  where
    switchExpBody = many switchExpBodyBranch
    switchExpBodyBranch = do
      lbl <- switchLabelNew
      body <-
        (SwitchExpBranchBlock <$> noLoc (braces (list blockStmt)))
          <|> (SwitchExpBranchBlock <$> try (blockStmt >>= \s -> return [s]))
          <|> (SwitchExpBranchExp <$> branchExp)
      return $ SwitchExpBranch lbl body
    branchExp = do
      e <- exp
      semiColon
      return e

-- Try-catch clauses

catch :: P Catch
catch = do
  tok KW_Catch
  fp <- parens formalParam
  b <- noLoc block
  return (Catch fp b)

tryResourceList :: P [TryResource]
tryResourceList = do
  l <- opt $
    parens $ do
      l <- seplist tryResource semiColon
      _ <- opt semiColon
      return l
  case l of
    Just xs -> return xs
    Nothing -> return []
  where
    tryResource =
      (TryResourceVarDecl <$> try resourceDecl)
        <|> (TryResourceQualAccess <$> try fieldAccess)
        <|> (TryResourceVarAccess <$> ident)

resourceDecl :: P ResourceDecl
resourceDecl = do
  ms <- list modifier
  typ <- ttype
  vid <- varDeclId
  tok Op_Equal
  val <- varInit
  return $ ResourceDecl ms typ vid val

----------------------------------------------------------------------------
-- Expressions

stmtExp :: P Exp
stmtExp =
  try preIncDec
    <|> try postIncDec
    <|> try assignment
    -- There are sharing gains to be made by unifying these two
    <|> try methodInvocationExp
    <|> try lambdaExp
    <|> try methodRef
    <|> instanceCreation

preIncDec :: P Exp
preIncDec = do
  op <- preIncDecOp
  e <- unaryExp
  return $ op e

postIncDec :: P Exp
postIncDec = do
  e <- postfixExpNES
  ops <- list1 postfixOp
  return $ foldl (\a s -> s a) e ops

assignment :: P Exp
assignment = do
  startLoc <- getLocation
  lh <- lhs
  op <- assignOp
  e <- assignExp
  endLoc <- getLocation
  return (Assign (startLoc, endLoc) lh op e)

lhs :: P Lhs
lhs =
  try (FieldLhs <$> fieldAccess)
    <|> try (ArrayLhs <$> arrayAccess)
    <|> NameLhs <$> name

exp :: P Exp
exp = assignExp

assignExp :: P Exp
assignExp = try switchExp <|> try methodRef <|> try lambdaExp <|> try assignment <|> condExp

condExp :: P Exp
condExp = do
  startLoc <- getLocation
  ie <- infixExp
  ces <- list (condExpSuffix startLoc)
  return $ foldl (\a s -> s a) ie ces

condExpSuffix :: Location -> P (Exp -> Exp)
condExpSuffix startLoc = do
  tok Op_Query
  th <- exp
  colon
  el <- condExp
  endLoc <- getLocation
  return $ \ce -> Cond (startLoc, endLoc) ce th el

infixExp :: P Exp
infixExp = do
  ue <- unaryExp
  ies <- list infixExpSuffix
  return $ foldl (\a s -> s a) ue ies

infixExpSuffix :: P (Exp -> Exp)
infixExpSuffix =
  ( do
      op <- infixCombineOp
      ie2 <- infixExp
      return $ \ie1 -> BinOp ie1 op ie2
  )
    <|> ( do
            op <- infixOp
            e2 <- unaryExp
            return $ \e1 -> BinOp e1 op e2
        )
    <|> ( do
            tok KW_Instanceof
            t <- refType
            mName <- opt name
            return $ \e1 -> InstanceOf e1 t mName
        )

unaryExp :: P Exp
unaryExp =
  try preIncDec
    <|> try
      ( do
          op <- prefixOp
          ue <- unaryExp
          return $ op ue
      )
    <|> try
      ( do
          t <- parens ttype
          e <- unaryExp
          return $ Cast t e
      )
    <|> postfixExp

postfixExpNES :: P Exp
postfixExpNES =
  -- try postIncDec <|>
  try primary
    <|> ExpName <$> name

postfixExp :: P Exp
postfixExp = do
  pe <- postfixExpNES
  ops <- list postfixOp
  return $ foldl (\a s -> s a) pe ops

primary :: P Exp
primary = primaryNPS |>> primarySuffix

primaryNPS :: P Exp
primaryNPS = try arrayCreation <|> primaryNoNewArrayNPS

primaryNoNewArray = startSuff primaryNoNewArrayNPS primarySuffix

primaryNoNewArrayNPS :: P Exp
primaryNoNewArrayNPS =
  Lit <$> literal
    <|> const This <$> tok KW_This
    <|> parens exp
    <|>
    -- TODO: These two following should probably be merged more
    ( try $ do
        rt <- resultType
        period >> tok KW_Class
        return $ ClassLit rt
    )
    <|> ( try $ do
            n <- name
            period >> tok KW_This
            return $ ThisClass n
        )
    <|> try instanceCreationNPS
    <|> try (MethodInv <$> methodInvocationNPS)
    <|> try (FieldAccess <$> fieldAccessNPS)
    <|> ArrayAccess <$> arrayAccessNPS

primarySuffix :: P (Exp -> Exp)
primarySuffix =
  try instanceCreationSuffix
    <|> try ((ArrayAccess .) <$> arrayAccessSuffix)
    <|> try ((MethodInv .) <$> methodInvocationSuffix)
    <|> (FieldAccess .) <$> fieldAccessSuffix

instanceCreationNPS :: P Exp
instanceCreationNPS =
  do
    tok KW_New
    tas <- lopt typeArgs
    tds <- typeDeclSpecifier
    as <- args
    mcb <- opt (noLoc classBody)
    return $ InstanceCreation tas tds as mcb

typeDeclSpecifier :: P TypeDeclSpecifier
typeDeclSpecifier =
  ( try $ do
      ct <- classType
      period
      i <- ident
      tok Op_LThan
      tok Op_GThan
      return $ TypeDeclSpecifierWithDiamond ct i Diamond
  )
    <|> ( try $ do
            i <- ident
            tok Op_LThan
            tok Op_GThan
            return $ TypeDeclSpecifierUnqualifiedWithDiamond i Diamond
        )
    <|> ( do
            ct <- classType
            return $ TypeDeclSpecifier ct
        )

instanceCreationSuffix :: P (Exp -> Exp)
instanceCreationSuffix =
  do
    period >> tok KW_New
    tas <- lopt typeArgs
    i <- ident
    as <- args
    mcb <- opt (noLoc classBody)
    return $ \p -> QualInstanceCreation p tas i as mcb

instanceCreation :: P Exp
instanceCreation =
  try instanceCreationNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let icp = foldl (\a s -> s a) p ss
    case icp of
      QualInstanceCreation {} -> return icp
      _ -> fail ""

lambdaParams :: P LambdaParams
lambdaParams =
  try (LambdaSingleParam <$> ident)
    <|> try (parens $ LambdaFormalParams <$> (seplist formalParam comma))
    <|> (parens $ LambdaInferredParams <$> (seplist ident comma))

lambdaExp :: P Exp
lambdaExp =
  Lambda
    <$> (lambdaParams <* (tok LambdaArrow))
    <*> ( (LambdaBlock <$> (try blockNoLoc))
            <|> (LambdaExpression <$> exp)
        )

methodRef :: P Exp
methodRef = do
  n <- name
  tok MethodRefSep
  target <-
    (tok KW_New >> return MethodRefConstructor)
      <|> (MethodRefIdent <$> ident)
  return $ MethodRef n target

{-
instanceCreation =
    (do tok KW_New
        tas <- lopt typeArgs
        ct  <- classType
        as  <- args
        mcb <- opt classBody
        return $ InstanceCreation tas ct as mcb) <|>
    (do p   <- primary
        period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ QualInstanceCreation p tas i as mcb)
-}

fieldAccessNPS :: P FieldAccess
fieldAccessNPS =
  ( do
      tok KW_Super >> period
      i <- ident
      return $ SuperFieldAccess i
  )
    <|> ( do
            n <- name
            period >> tok KW_Super >> period
            i <- ident
            return $ ClassFieldAccess n i
        )

fieldAccessSuffix :: P (Exp -> FieldAccess)
fieldAccessSuffix = do
  period
  i <- ident
  return $ \p -> PrimaryFieldAccess p i

fieldAccess :: P FieldAccess
fieldAccess =
  try fieldAccessNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let fap = foldl (\a s -> s a) p ss
    case fap of
      FieldAccess fa -> return fa
      _ -> fail ""

{-
fieldAccess :: P FieldAccess
fieldAccess = try fieldAccessNPS <|> do
    p <- primary
    fs <- fieldAccessSuffix
    return (fs p)
-}

{-
fieldAccess :: P FieldAccess
fieldAccess =
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    (try $ do
        n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i) <|>
    (do p <- primary
        period
        i <- ident
        return $ PrimaryFieldAccess p i)
-}

methodInvocationNPS :: P MethodInvocation
methodInvocationNPS =
  ( do
      tok KW_Super >> period
      rts <- lopt refTypeArgs
      i <- ident
      as <- args
      return $ SuperMethodCall rts i as
  )
    <|> ( do
            n <- name
            f <-
              ( do
                  as <- args
                  return $ \n -> MethodCall n as
                )
                <|> ( period >> do
                        msp <- opt (tok KW_Super >> period)
                        rts <- lopt refTypeArgs
                        i <- ident
                        as <- args
                        let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                        return $ \n -> mc n rts i as
                    )
            return $ f n
        )

methodInvocationSuffix :: P (Exp -> MethodInvocation)
methodInvocationSuffix = do
  period
  rts <- lopt refTypeArgs
  i <- ident
  as <- args
  return $ \p -> PrimaryMethodCall p [] i as

methodInvocationExp :: P Exp
methodInvocationExp =
  try
    ( do
        p <- primaryNPS
        ss <- list primarySuffix
        let mip = foldl (\a s -> s a) p ss
        case mip of
          MethodInv _ -> return mip
          _ -> fail ""
    )
    <|> (MethodInv <$> methodInvocationNPS)

{-
methodInvocation :: P MethodInvocation
methodInvocation =
    (do tok KW_Super >> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ SuperMethodCall rts i as) <|>
    (do p <- primary
        period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ PrimaryMethodCall p rts i as) <|>
    (do n <- name
        f <- (do as <- args
                 return $ \n -> MethodCall n as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                return $ \n -> mc n rts i as)
        return $ f n)
-}

args :: P [Argument]
args = parens $ seplist exp comma

-- Arrays

arrayAccessNPS :: P ArrayIndex
arrayAccessNPS = do
  n <- name
  e <- list1 $ brackets exp
  return $ ArrayIndex (ExpName n) e

arrayAccessSuffix :: P (Exp -> ArrayIndex)
arrayAccessSuffix = do
  e <- list1 $ brackets exp
  return $ \ref -> ArrayIndex ref e

arrayAccess =
  try arrayAccessNPS <|> do
    p <- primaryNoNewArrayNPS
    ss <- list primarySuffix
    let aap = foldl (\a s -> s a) p ss
    case aap of
      ArrayAccess ain -> return ain
      _ -> fail ""

{-
arrayAccess :: P (Exp, Exp)
arrayAccess = do
    ref <- arrayRef
    e   <- brackets exp
    return (ref, e)

arrayRef :: P Exp
arrayRef = ExpName <$> name <|> primaryNoNewArray
-}

arrayCreation :: P Exp
arrayCreation = do
  tok KW_New
  t <- nonArrayType
  f <-
    ( try $ do
        ds <- list1 $ brackets empty
        ai <- arrayInit
        return $ \t -> ArrayCreateInit t (length ds) ai
      )
      <|> ( do
              des <- list1 $ try $ brackets exp
              ds <- list $ brackets empty
              return $ \t -> ArrayCreate t des (length ds)
          )
  return $ f t

literal :: P Literal
literal =
  javaToken $ \t -> case t of
    IntTok i -> Just (Int i)
    LongTok l -> Just (Word l)
    DoubleTok d -> Just (Double d)
    FloatTok f -> Just (Float f)
    CharTok c -> Just (Char c)
    StringTok s -> Just (String s)
    BoolTok b -> Just (Boolean b)
    NullTok -> Just Null
    _ -> Nothing

-- Operators

preIncDecOp, prefixOp, postfixOp :: P (Exp -> Exp)
preIncDecOp = do
  startLoc <- getLocation
  ( do
      tok Op_PPlus
      endLoc <- getLocation
      return (PreIncrement (startLoc, endLoc))
    )
    <|> ( do
            tok Op_MMinus
            endLoc <- getLocation
            return (PreDecrement (startLoc, endLoc))
        )
prefixOp =
  (tok Op_Bang >> return PreNot)
    <|> (tok Op_Tilde >> return PreBitCompl)
    <|> (tok Op_Plus >> return PrePlus)
    <|> (tok Op_Minus >> return PreMinus)
postfixOp =
  (tok Op_PPlus >> return PostIncrement)
    <|> (tok Op_MMinus >> return PostDecrement)

assignOp :: P AssignOp
assignOp =
  (tok Op_Equal >> return EqualA)
    <|> (tok Op_StarE >> return MultA)
    <|> (tok Op_SlashE >> return DivA)
    <|> (tok Op_PercentE >> return RemA)
    <|> (tok Op_PlusE >> return AddA)
    <|> (tok Op_MinusE >> return SubA)
    <|> (tok Op_LShiftE >> return LShiftA)
    <|> (tok Op_RShiftE >> return RShiftA)
    <|> (tok Op_RRShiftE >> return RRShiftA)
    <|> (tok Op_AndE >> return AndA)
    <|> (tok Op_CaretE >> return XorA)
    <|> (tok Op_OrE >> return OrA)

infixCombineOp :: P Op
infixCombineOp =
  (tok Op_And >> return And)
    <|> (tok Op_Caret >> return Xor)
    <|> (tok Op_Or >> return Or)
    <|> (tok Op_AAnd >> return CAnd)
    <|> (tok Op_OOr >> return COr)

infixOp :: P Op
infixOp =
  (tok Op_Star >> return Mult)
    <|> (tok Op_Slash >> return Div)
    <|> (tok Op_Percent >> return Rem)
    <|> (tok Op_Plus >> return Add)
    <|> (tok Op_Minus >> return Sub)
    <|> (tok Op_LShift >> return LShift)
    <|> (tok Op_LThan >> return LThan)
    <|> ( try $ do
            tok Op_GThan
            tok Op_GThan
            tok Op_GThan
            return RRShift
        )
    <|> ( try $ do
            tok Op_GThan
            tok Op_GThan
            return RShift
        )
    <|> (tok Op_GThan >> return GThan)
    <|> (tok Op_LThanE >> return LThanE)
    <|> (tok Op_GThanE >> return GThanE)
    <|> (tok Op_Equals >> return Equal)
    <|> (tok Op_BangE >> return NotEq)

----------------------------------------------------------------------------
-- Types

ttype :: P Type
ttype = try (RefType <$> refType) <|> PrimType <$> primType

primType :: P PrimType
primType =
  tok KW_Boolean
    >> return BooleanT
      <|> tok KW_Byte
    >> return ByteT
      <|> tok KW_Short
    >> return ShortT
      <|> tok KW_Int
    >> return IntT
      <|> tok KW_Long
    >> return LongT
      <|> tok KW_Char
    >> return CharT
      <|> tok KW_Float
    >> return FloatT
      <|> tok KW_Double
    >> return DoubleT

refType :: P RefType
refType =
  ( do
      pt <- primType
      (_ : bs) <- list1 arrBrackets
      return $
        foldl
          (\f _ -> ArrayType . RefType . f)
          (ArrayType . PrimType)
          bs
          pt
  )
    <|> ( do
            ct <- classType
            bs <- list arrBrackets
            return $
              foldl
                (\f _ -> ArrayType . RefType . f)
                ClassRefType
                bs
                ct
        )
    <?> "refType"

nonArrayType :: P Type
nonArrayType =
  PrimType <$> primType
    <|> RefType <$> ClassRefType <$> classType

classType :: P ClassType
classType = ClassType <$> seplist1 classTypeSpec period

classTypeSpec :: P (Ident, [TypeArgument])
classTypeSpec = do
  i <- ident
  tas <- lopt typeArgs
  return (i, tas)

resultType :: P (Maybe Type)
resultType = tok KW_Void >> return Nothing <|> Just <$> ttype <?> "resultType"

refTypeList :: P [RefType]
refTypeList = seplist1 refType comma

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParams :: P [TypeParam]
typeParams = angles $ seplist1 typeParam comma

typeParam :: P TypeParam
typeParam = do
  i <- ident
  bs <- lopt bounds
  return $ TypeParam i bs

bounds :: P [RefType]
bounds = tok KW_Extends >> seplist1 refType (tok Op_And)

typeArgs :: P [TypeArgument]
typeArgs = angles $ seplist1 typeArg comma

typeArg :: P TypeArgument
typeArg =
  tok Op_Query
    >> Wildcard <$> opt wildcardBound
      <|> ActualType <$> refType

wildcardBound :: P WildcardBound
wildcardBound =
  tok KW_Extends
    >> ExtendsBound <$> refType
      <|> tok KW_Super
    >> SuperBound <$> refType

refTypeArgs :: P [RefType]
refTypeArgs = angles refTypeList

----------------------------------------------------------------------------
-- Names

name :: P Name
name = Name <$> seplist1 ident period

ident :: P Ident
ident = javaToken $ \t -> case t of
  IdentTok s -> Just (Ident s)
  _ -> Nothing

fixedIdent :: String -> a -> P a
fixedIdent fixed result = javaToken $ \t -> case t of
  IdentTok s | s == fixed -> Just result
  _ -> Nothing

------------------------------------------------------------

empty :: P ()
empty = return ()

opt :: P a -> P (Maybe a)
opt = optionMaybe

bopt :: P a -> P Bool
bopt p = opt p >>= \ma -> return $ isJust ma

optList :: P [a] -> P [a]
optList p = do
  mx <- opt p
  case mx of
    Just l -> return l
    Nothing -> return []

lopt :: P [a] -> P [a]
lopt p = do
  mas <- opt p
  case mas of
    Nothing -> return []
    Just as -> return as

list :: P a -> P [a]
list = option [] . list1

list1 :: P a -> P [a]
list1 = many1

seplist :: P a -> P sep -> P [a]
-- seplist = sepBy
seplist p sep = option [] $ seplist1 p sep

seplist1 :: P a -> P sep -> P [a]
-- seplist1 = sepBy1
seplist1 p sep =
  p >>= \a ->
    try
      ( do
          sep
          as <- seplist1 p sep
          return (a : as)
      )
      <|> return [a]

startSuff, (|>>) :: P a -> P (a -> a) -> P a
startSuff start suffix = do
  x <- start
  ss <- list suffix
  return $ foldl (\a s -> s a) x ss
(|>>) = startSuff

------------------------------------------------------------

javaToken :: (Token -> Maybe a) -> P a
javaToken test = token showT posT testT
  where
    showT (L _ t) = show t
    posT (L p _) = pos2sourcePos p
    testT (L _ t) = test t

tok, matchToken :: Token -> P ()
tok = matchToken
matchToken t = javaToken (\r -> if r == t then Just () else Nothing)

pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos (l, c) = newPos "" l c

type Mod a = Location -> [Modifier] -> a

parens, brackets, angles :: P a -> P a
parens = between (tok OpenParen) (tok CloseParen)
brackets = between (tok OpenSquare) (tok CloseSquare)
angles = between (tok Op_LThan) (tok Op_GThan)

braces :: P a -> P (a, Location)
braces p = do
  _ <- tok OpenCurly
  x <- p
  endLoc <- getLocation
  _ <- tok CloseCurly
  pure (x, endLoc)

endSemi :: P a -> P (a, Location)
endSemi p = do
  a <- p
  semiColon
  endLoc <- getLocation
  return (a, endLoc)

comma, colon, semiColon, period :: P ()
comma = tok Comma
colon = tok Op_Colon
semiColon = tok SemiColon
period = tok Period
