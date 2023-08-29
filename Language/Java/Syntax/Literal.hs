module Language.Java.Syntax.Literal (isString, string) where

import Language.Java.Syntax (Literal (..))

isString :: Literal -> Bool
isString (String _ _) = True
isString _ = False

string :: Literal -> Maybe String
string (String _ str) = Just str
string _ = Nothing
