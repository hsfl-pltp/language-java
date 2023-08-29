module Language.Java.Syntax.Literal (isString, string) where

isString :: Literal -> Bool
isString (String _ _) = True
isString _ = False

string :: Literal -> String
string (String _ str) = str
string _ = []
