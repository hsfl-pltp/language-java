module Language.Java.Syntax.Modifier
  ( isFinal,
    isPrivate,
    isProtected,
    isPublic,
    isStatic,
  )
where

import Language.Java.Syntax (Modifier (..))

isPublic :: Modifier p -> Bool
isPublic (Public _) = True
isPublic _ = False

isStatic :: Modifier p -> Bool
isStatic (Static _) = True
isStatic _ = False

isProtected :: Modifier p -> Bool
isProtected (Protected _) = True
isProtected _ = False

isPrivate :: Modifier p -> Bool
isPrivate (Private _) = True
isPrivate _ = False

isFinal :: Modifier p -> Bool
isFinal (Final _) = True
isFinal _ = False
