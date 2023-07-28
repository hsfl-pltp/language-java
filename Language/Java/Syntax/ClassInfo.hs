module Language.Java.Syntax.ClassInfo
  ( ClassInfo (..),
    hasField,
    hasIdent,
    fromClassDecl,
  )
where

import Data.Maybe ( mapMaybe )
import Language.Java.Syntax
import Language.Java.Syntax.Decl as Decl (memberDecl)
import Language.Java.Syntax.MemberDecl as MemberDecl (classDecl, fields)
import Language.Java.Syntax.VarDecl as VarDecl (ident)

-- | data type used to pass down icClassInfos structure in the file
-- Vielleicht classInfo oder javaClass {ciFields: :, ciInnerClasses}
data ClassInfo = ClassInfo
  { ciFields :: [Ident],
    ciInnerClasses :: [ClassInfo],
    ciIdent :: Ident
  }

hasField :: Ident -> ClassInfo -> Bool
hasField idnt classInfo = any (eq IgnoreSourceSpan idnt) (ciFields classInfo)

hasIdent :: Ident -> ClassInfo -> Bool
hasIdent idnt classInfo = eq IgnoreSourceSpan idnt (ciIdent classInfo)

fromClassDecl :: ClassDecl p -> ClassInfo
fromClassDecl (ClassDecl _ _ idnt _ _ _ (ClassBody decls)) =
  ClassInfo
    ((concat . mapMaybe declFieldIdents) decls)
    (map fromClassDecl (mapMaybe declClassDecl decls))
    idnt
fromClassDecl (RecordDecl _ _ idnt _ _ _ (ClassBody decls)) =
  ClassInfo
    ((concat . mapMaybe declFieldIdents) decls)
    (map fromClassDecl (mapMaybe declClassDecl decls))
    idnt
fromClassDecl (EnumDecl _ _ idnt _ (EnumBody cons decls)) =
  ClassInfo
    ((concat . mapMaybe declFieldIdents) decls ++ map (\(EnumConstant idnt' _ _) -> idnt') cons)
    (map fromClassDecl (mapMaybe declClassDecl decls))
    idnt

declClassDecl :: Decl p -> Maybe (ClassDecl p)
declClassDecl decl =
  Decl.memberDecl decl
    >>= MemberDecl.classDecl

declFieldIdents :: Decl p -> Maybe [Ident]
declFieldIdents decl =
  fmap
    (map VarDecl.ident)
    ( Decl.memberDecl decl
        >>= MemberDecl.fields
    )
