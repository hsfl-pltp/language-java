{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Java.SourceSpan 
  ( Location (..),
    SourceSpan,
    dummyLocation,
    dummySourceSpan,
    locationEof,
    isEof
  ) 
where
    
import Data.Data
import GHC.Generics (Generic)

type SourceSpan = (Location, Location)

data Location = Location
  { loc_file :: FilePath,
    loc_line :: Int,
    loc_column :: Int
  }
  deriving (Eq, Show, Read, Typeable, Generic, Data)

dummyLocation :: Location
dummyLocation = Location "<input>" 1 1

dummySourceSpan :: SourceSpan
dummySourceSpan = (dummyLocation, dummyLocation)

locationEof :: Location
locationEof = Location "" 0 0

isEof :: Location -> Bool
isEof loc = loc == locationEof
