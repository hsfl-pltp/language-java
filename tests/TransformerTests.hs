module TransformerTests where

import Data.Generics.Uniplate.Data (universeBi)
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
          Left _ -> assertFailure ""
          Right cUnit -> do
            let classifiedNames = universeBi (analyze [] cUnit)
            case checkExpressionNames classifiedNames of
              Left string -> assertFailure string
              Right _ -> return ()
    )

checkExpressionNames :: [ClassifiedName] -> Either String ()
checkExpressionNames [] = Right ()
checkExpressionNames ((ExpressionName _) : xs) = checkExpressionNames xs
checkExpressionNames (classifiedName : _) = Left (show (sourceSpan classifiedName))