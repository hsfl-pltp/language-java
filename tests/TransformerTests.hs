{-# LANGUAGE LambdaCase #-}

module TransformerTests where

import Data.Generics.Uniplate.Data (universeBi)
import Data.List (intercalate)
import Language.Java.Parser
import Language.Java.SourceSpan (Located (sourceSpan))
import Language.Java.Syntax (ClassifiedName (..))
import Language.Java.Transformer
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

expressionFile :: FilePath
expressionFile = "tests" </> "java" </> "transformer" </> "ExpressionName.java"

typeNameFile :: FilePath
typeNameFile = "tests" </> "java" </> "transformer" </> "TypeName.java"

allTransformerTests :: [TestTree]
allTransformerTests = [expressionNameTests, typeNameTests]

createTestTree :: String -> FilePath -> (ClassifiedName -> Bool) -> TestTree
createTestTree testName filepath predicate =
  testCase
    testName
    ( do
        result <- parser compilationUnit filepath <$> readFile filepath
        case result of
          Left parseError -> assertFailure (show parseError)
          Right cUnit -> do
            let classifiedNames = universeBi (analyzeCompilationUnit cUnit)
            case filter
              predicate
              classifiedNames of
              [] -> return ()
              xs -> assertFailure (intercalate "\n" (map (show . sourceSpan) xs))
    )

expressionNameTests :: TestTree
expressionNameTests =
  createTestTree
    "ExpressionName"
    expressionFile
    ( \case
        (ExpressionName _) -> False
        _ -> True
    )

typeNameTests :: TestTree
typeNameTests =
  createTestTree
    "TypeName"
    typeNameFile
    ( \case
        (TypeName _) -> False
        _ -> True
    )
