module AnalyzerTests where

import Control.Monad (MonadPlus (mzero))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Parser
import Language.Java.Syntax (CompilationUnit, Parsed, TryResource (TryResourceQualAccess), ClassifiedName (TypeName))
import Language.Java.Syntax.Types (ClassifiedName (ExpressionName))
import Language.Java.Transformer
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

expressionFile :: FilePath
expressionFile = "tests" </> "java" </> "ExpressionName.java"

allAnalyzerTests :: [TestTree]
allAnalyzerTests = [expressionNameTests]

expressionNameTests :: TestTree
expressionNameTests =
  testCase
    "ExressionName"
    ( do
        cUnit <- parser compilationUnit expressionFile <$> readFile expressionFile
        case cUnit of
          Left _ -> assertFailure ""
          Right cUnit -> do
            let classifiedNames = universeBi (analyze [] cUnit)
            assertBool "not classified Correctly" (all isExpressionName classifiedNames)
    )

isExpressionName :: ClassifiedName -> Bool
isExpressionName (ExpressionName _) = True
isExpressionName _ = False

checkExpressionNames :: [ClassifiedName] -> Either String ()
checkExpressionNames [] = Right ()
checkExpressionNames ((ExpressionName _) : xs) = checkExpressionNames xs
checkExpressionNames ((TypeName name) : _) = Left (show (sourceSpan name))