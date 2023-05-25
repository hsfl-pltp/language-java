{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception as E
import Control.Monad
import Data.List (isSuffixOf)
import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.SourceSpan
import Language.Java.Syntax
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

instance Arbitrary (CompilationUnit p) where
  arbitrary = CompilationUnit <$> arbitrary <*> arbitrary <*> ((: []) <$> arbitrary)

instance Arbitrary PackageDecl where
  arbitrary = PackageDecl <$> arbitrary

instance Arbitrary ImportDecl where
  arbitrary = ImportDecl dummySourceSpan <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (TypeDecl p) where
  arbitrary = ClassTypeDecl <$> arbitrary

instance Arbitrary (ClassDecl p) where
  arbitrary = (ClassDecl dummySourceSpan [] <$> arbitrary) <*> pure [] <*> pure Nothing <*> pure [] <*> arbitrary

instance Arbitrary (ClassBody p) where
  arbitrary = pure (ClassBody [])

instance Arbitrary Name where
  arbitrary = Name dummySourceSpan <$> (choose (1, 3) >>= \len -> replicateM len arbitrary)

instance Arbitrary Ident where
  arbitrary = Ident dummySourceSpan . unkeyword <$> (choose (1, 15) >>= \len -> replicateM len (elements (['a' .. 'z'] ++ ['A' .. 'Z'])))
    where
      unkeyword k
        | k `elem` ["if", "do", "then", "else"] = "x" ++ k
        | otherwise = k

----------------------------------------------------------
testJavaDirectory :: FilePath
testJavaDirectory = "tests" </> "java"

isJavaFile :: FilePath -> Bool
isJavaFile f = ".java" `isSuffixOf` f

toTestCase :: ParserMode -> Bool -> FilePath -> TestTree
toTestCase mode expected jFile = testCase (takeBaseName jFile) doTest
  where
    doTest = do
      r <- E.try parseOne
      case r of
        Left (e :: E.SomeException) -> assertBool ("failure exception: " ++ show e) (not expected)
        Right (Left perr) -> assertBool ("failure parse error: " ++ show perr) (not expected)
        Right (Right p) -> assertBool ("success: " ++ show p) expected
    parseOne = parserWithMode mode compilationUnit jFile <$> readFile jFile

getAllJavaPaths :: FilePath -> IO [FilePath]
getAllJavaPaths path = map (path </>) . filter isJavaFile <$> getDirectoryContents path

main :: IO ()
main = do
  exists <- doesDirectoryExist testJavaDirectory
  unless exists $ error "cannot find tests files java directory"

  allGoodJavas <- getAllJavaPaths (testJavaDirectory </> "good")
  allBadJavas <- getAllJavaPaths (testJavaDirectory </> "bad")
  let -- the bad tests that work with shallow parsing
      shallowGoodJavas = [testJavaDirectory </> "bad" </> "DiamondIncorrectPlacement.java"]
      shallowBadJavas = filter (`notElem` shallowGoodJavas) allBadJavas

  defaultMain $
    testGroup
      "java"
      [ testGroup "parsing unit good" (map (toTestCase ParseFull True) allGoodJavas),
        testGroup
          "parsing shallow unit good"
          (map (toTestCase ParseShallow True) (allGoodJavas ++ shallowGoodJavas)),
        testGroup "parsing unit bad" (map (toTestCase ParseFull False) allBadJavas),
        testGroup "parsing shallow unit bad" (map (toTestCase ParseShallow False) shallowBadJavas),
        testProperty
          "parsing.generating==id"
          ( \g -> case parserWithState (ParserState ParseFull False) compilationUnit "<input>" (show $ pretty g) of
              Right g' -> eq IgnoreSourceSpan g g'
              Left perr -> error (show (pretty g) ++ show perr)
          )
      ]
