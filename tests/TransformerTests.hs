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

allTransformerTests :: [TestTree]
allTransformerTests = [expressionNameTests]

expressionNameTests :: TestTree
expressionNameTests =
  testCase
    "ExressionName"
    ( do
        result <- parser compilationUnit expressionFile <$> readFile expressionFile
        case result of
          Left parseError -> assertFailure (show parseError)
          Right cUnit -> do
            let classifiedNames = universeBi (analyzeCompilationUnit cUnit)
            case filter
              ( \case
                  (ExpressionName _) -> False
                  _ -> True
              )
              classifiedNames of
              [] -> return ()
              xs -> assertFailure (intercalate "\n" (map (show . sourceSpan) xs))
    )