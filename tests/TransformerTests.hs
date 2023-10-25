module TransformerTests where

import Data.Generics.Uniplate.Data (universeBi)
import Data.List (intercalate)
import Language.Java.Parser
import Language.Java.Syntax (ClassifiedName (..))
import qualified Language.Java.Syntax.ClassifiedName as ClassifiedName
import Language.Java.Transformer
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

basepath :: FilePath
basepath = "tests" </> "java" </> "transformer"

fieldFile :: FilePath
fieldFile = basepath </> "Field.java"

expressionFile :: FilePath
expressionFile = basepath </> "ExpressionName.java"

typeNameFile :: FilePath
typeNameFile = basepath </> "TypeName.java"

unknownFile :: FilePath
unknownFile = basepath </> "Unknown.java"

allTransformerTests :: [TestTree]
allTransformerTests =
  [ createTestTree "Field" fieldFile (not . ClassifiedName.isField),
    createTestTree "ExpressionName" expressionFile (not . ClassifiedName.isExpressionName),
    createTestTree "TypeName" typeNameFile (not . ClassifiedName.isTypeName),
    createTestTree "Unknown" unknownFile (not . ClassifiedName.isUnknownName)
  ]

createTestTree :: String -> FilePath -> (ClassifiedName -> Bool) -> TestTree
createTestTree testName filepath predicate =
  testCase
    testName
    ( do
        result <- parser compilationUnit filepath <$> readFile filepath
        case result of
          Left parseError -> assertFailure (show parseError)
          Right cUnit -> do
            let classifiedNames = universeBi (transformCompilationUnitToAnalyzed cUnit)
            case filter predicate classifiedNames of
              [] -> return ()
              xs -> assertFailure (intercalate "\n" (map show xs))
    )
