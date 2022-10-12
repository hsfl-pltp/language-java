{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-unused-binds #-}
module Language.Java.Lexer (L(..), Token(..), lexer) where

import Numeric
import Data.Char
}

%wrapper "posn"

$digit       = [0-9]
$digitUnder  = [0-9_]
$nonzero     = [1-9]
$octdig      = [0-7]
$hexdig      = [0-9A-Fa-f]
$hexdigUnder = [0-9A-Fa-f_]

@lineterm = [\n\r] | \r\n

-- TODO: this doesn't notice a comment that ends "**/"
@tradcomm = "/*" ( ~[\*] | \*+ (~[\/\*] | \n) | \n )* \*+ "/"
@linecomm = "//" .* @lineterm
@comm = @tradcomm | @linecomm

$javaLetter = [a-zA-Z\_\$]
$javaLetterOrDigit = [a-zA-Z0-9\_\$]

@octEscape = [0123]? $octdig{1,2}
@hexEscape = u $hexdig{4}
@charEscape = \\ (@octEscape | @hexEscape | [btnfr\"\'\\])

@expsuffix = [\+\-]? $digit+
@exponent = [eE] @expsuffix
@pexponent = [pP] @expsuffix

-- result type is TokResult
tokens  :-

    $white+         ;
    @comm           ;

    "@interface"    { \p _ -> L (mkPos p) $ Right KW_AnnInterface }
    abstract        { \p _ -> L (mkPos p) $ Right KW_Abstract     }
    assert          { \p _ -> L (mkPos p) $ Right KW_Assert       }
    boolean         { \p _ -> L (mkPos p) $ Right KW_Boolean      }
    break           { \p _ -> L (mkPos p) $ Right KW_Break        }
    byte            { \p _ -> L (mkPos p) $ Right KW_Byte         }
    case            { \p _ -> L (mkPos p) $ Right KW_Case         }
    catch           { \p _ -> L (mkPos p) $ Right KW_Catch        }
    char            { \p _ -> L (mkPos p) $ Right KW_Char         }
    class           { \p _ -> L (mkPos p) $ Right KW_Class        }
    const           { \p _ -> L (mkPos p) $ Right KW_Const        }
    continue        { \p _ -> L (mkPos p) $ Right KW_Continue     }
    default         { \p _ -> L (mkPos p) $ Right KW_Default      }
    do              { \p _ -> L (mkPos p) $ Right KW_Do           }
    double          { \p _ -> L (mkPos p) $ Right KW_Double       }
    else            { \p _ -> L (mkPos p) $ Right KW_Else         }
    enum            { \p _ -> L (mkPos p) $ Right KW_Enum         }
    extends         { \p _ -> L (mkPos p) $ Right KW_Extends      }
    final           { \p _ -> L (mkPos p) $ Right KW_Final        }
    finally         { \p _ -> L (mkPos p) $ Right KW_Finally      }
    float           { \p _ -> L (mkPos p) $ Right KW_Float        }
    for             { \p _ -> L (mkPos p) $ Right KW_For          }
    goto            { \p _ -> L (mkPos p) $ Right KW_Goto         }
    if              { \p _ -> L (mkPos p) $ Right KW_If           }
    implements      { \p _ -> L (mkPos p) $ Right KW_Implements   }
    import          { \p _ -> L (mkPos p) $ Right KW_Import       }
    instanceof      { \p _ -> L (mkPos p) $ Right KW_Instanceof   }
    int             { \p _ -> L (mkPos p) $ Right KW_Int          }
    interface       { \p _ -> L (mkPos p) $ Right KW_Interface    }
    long            { \p _ -> L (mkPos p) $ Right KW_Long         }
    native          { \p _ -> L (mkPos p) $ Right KW_Native       }
    new             { \p _ -> L (mkPos p) $ Right KW_New          }
    package         { \p _ -> L (mkPos p) $ Right KW_Package      }
    private         { \p _ -> L (mkPos p) $ Right KW_Private      }
    protected       { \p _ -> L (mkPos p) $ Right KW_Protected    }
    public          { \p _ -> L (mkPos p) $ Right KW_Public       }
    record          { \p _ -> L (mkPos p) $ Right KW_Record       }
    return          { \p _ -> L (mkPos p) $ Right KW_Return       }
    short           { \p _ -> L (mkPos p) $ Right KW_Short        }
    static          { \p _ -> L (mkPos p) $ Right KW_Static       }
    strictfp        { \p _ -> L (mkPos p) $ Right KW_Strictfp     }
    super           { \p _ -> L (mkPos p) $ Right KW_Super        }
    switch          { \p _ -> L (mkPos p) $ Right KW_Switch       }
    synchronized    { \p _ -> L (mkPos p) $ Right KW_Synchronized }
    this            { \p _ -> L (mkPos p) $ Right KW_This         }
    throw           { \p _ -> L (mkPos p) $ Right KW_Throw        }
    throws          { \p _ -> L (mkPos p) $ Right KW_Throws       }
    transient       { \p _ -> L (mkPos p) $ Right KW_Transient    }
    try             { \p _ -> L (mkPos p) $ Right KW_Try          }
    void            { \p _ -> L (mkPos p) $ Right KW_Void         }
    volatile        { \p _ -> L (mkPos p) $ Right KW_Volatile     }
    while           { \p _ -> L (mkPos p) $ Right KW_While        }

    0               { \p _ -> L (mkPos p) $ Right $ IntTok 0        }
    0 [lL]          { \p _ -> L (mkPos p) $ Right $ LongTok 0       }
    0 $digitUnder+               { \p s -> parseNum p readOct s IntTok }
    0 $digitUnder+ [lL]          { \p s -> parseNum p readOct (init s) LongTok }
    $nonzero $digitUnder*        { \p s -> parseNum p reads s IntTok }
    $nonzero $digitUnder* [lL]   { \p s -> parseNum p reads (init s) LongTok }
    0 [xX] $hexdig $hexdigUnder*         { \p s -> parseNum p readHex (drop 2 s) IntTok }
    0 [xX] $hexdig $hexdigUnder* [lL]    { \p s -> parseNum p readHex (init (drop 2 s)) LongTok }

    $digit $digitUnder* \. $digit? $digitUnder* @exponent? [dD]?    { \p s -> parseNum p readFloat (dropDoubleSuffix s) DoubleTok }
            \. $digit $digitUnder* @exponent? [dD]?                 { \p s -> parseNum p readFloat ('0':(dropDoubleSuffix s)) DoubleTok }
    $digit $digitUnder* \. $digit? $digitUnder* @exponent? [fF]     { \p s -> parseNum p readFloat (init s) FloatTok }
            \. $digit $digitUnder* @exponent? [fF]                  { \p s -> parseNum p readFloat ('0':(init s)) FloatTok }
    $digit $digitUnder* @exponent                                   { \p s -> parseNum p readFloat s DoubleTok }
    $digit $digitUnder* @exponent? [dD]                             { \p s -> parseNum p readFloat (init s) DoubleTok }
    $digit $digitUnder* @exponent? [fF]                             { \p s -> parseNum p readFloat (init s) FloatTok }
    0 [xX] $hexdig? $hexdigUnder* \.? $hexdig? $hexdigUnder* @pexponent [dD]?     { \p s -> readHexExp p (drop 2 (dropDoubleSuffix s)) DoubleTok }
    0 [xX] $hexdig? $hexdigUnder* \.? $hexdig? $hexdigUnder* @pexponent [fF]      { \p s -> readHexExp p (drop 2 (init s)) FloatTok }

    true            { \p _ -> L (mkPos p) $ Right $ BoolTok True    }
    false           { \p _ -> L (mkPos p) $ Right $ BoolTok False   }

    ' (@charEscape | ~[\\\']) '               { \p s -> readCharTok p s }

    \" (@charEscape | ~[\\\"])* \"            { \p s -> readStringTok p s }

    null            {\p _ -> L (mkPos p) $ Right $ NullTok }

    $javaLetter $javaLetterOrDigit*     { \p s -> L (mkPos p) $ Right $ IdentTok s }

    \(              { \p _ -> L (mkPos p) $ Right $ OpenParen       }
    \)              { \p _ -> L (mkPos p) $ Right $ CloseParen      }
    \[              { \p _ -> L (mkPos p) $ Right $ OpenSquare      }
    \]              { \p _ -> L (mkPos p) $ Right $ CloseSquare     }
    \{              { \p _ -> L (mkPos p) $ Right $ OpenCurly       }
    \}              { \p _ -> L (mkPos p) $ Right $ CloseCurly      }
    \;              { \p _ -> L (mkPos p) $ Right $ SemiColon       }
    \,              { \p _ -> L (mkPos p) $ Right $ Comma           }
    \.              { \p _ -> L (mkPos p) $ Right $ Period          }
    "->"            { \p _ -> L (mkPos p) $ Right $ LambdaArrow     }
    "::"            { \p _ -> L (mkPos p) $ Right $ MethodRefSep    }

    "="             { \p _ -> L (mkPos p) $ Right $ Op_Equal        }
    ">"             { \p _ -> L (mkPos p) $ Right $ Op_GThan        }
    "<"             { \p _ -> L (mkPos p) $ Right $ Op_LThan        }
    "!"             { \p _ -> L (mkPos p) $ Right $ Op_Bang         }
    "~"             { \p _ -> L (mkPos p) $ Right $ Op_Tilde        }
    "?"             { \p _ -> L (mkPos p) $ Right $ Op_Query        }
    ":"             { \p _ -> L (mkPos p) $ Right $ Op_Colon        }
    "=="            { \p _ -> L (mkPos p) $ Right $ Op_Equals       }
    "<="            { \p _ -> L (mkPos p) $ Right $ Op_LThanE       }
    ">="            { \p _ -> L (mkPos p) $ Right $ Op_GThanE       }
    "!="            { \p _ -> L (mkPos p) $ Right $ Op_BangE        }
    "&&"            { \p _ -> L (mkPos p) $ Right $ Op_AAnd         }
    "||"            { \p _ -> L (mkPos p) $ Right $ Op_OOr          }
    "++"            { \p _ -> L (mkPos p) $ Right $ Op_PPlus        }
    "--"            { \p _ -> L (mkPos p) $ Right $ Op_MMinus       }
    "+"             { \p _ -> L (mkPos p) $ Right $ Op_Plus         }
    "-"             { \p _ -> L (mkPos p) $ Right $ Op_Minus        }
    "*"             { \p _ -> L (mkPos p) $ Right $ Op_Star         }
    "/"             { \p _ -> L (mkPos p) $ Right $ Op_Slash        }
    "&"             { \p _ -> L (mkPos p) $ Right $ Op_And          }
    "|"             { \p _ -> L (mkPos p) $ Right $ Op_Or           }
    "^"             { \p _ -> L (mkPos p) $ Right $ Op_Caret        }
    "%"             { \p _ -> L (mkPos p) $ Right $ Op_Percent      }
    "<<"            { \p _ -> L (mkPos p) $ Right $ Op_LShift       }
    "+="            { \p _ -> L (mkPos p) $ Right $ Op_PlusE        }
    "-="            { \p _ -> L (mkPos p) $ Right $ Op_MinusE       }
    "*="            { \p _ -> L (mkPos p) $ Right $ Op_StarE        }
    "/="            { \p _ -> L (mkPos p) $ Right $ Op_SlashE       }
    "&="            { \p _ -> L (mkPos p) $ Right $ Op_AndE         }
    "|="            { \p _ -> L (mkPos p) $ Right $ Op_OrE          }
    "^="            { \p _ -> L (mkPos p) $ Right $ Op_CaretE       }
    "%="            { \p _ -> L (mkPos p) $ Right $ Op_PercentE     }
    "<<="           { \p _ -> L (mkPos p) $ Right $ Op_LShiftE      }
    ">>="           { \p _ -> L (mkPos p) $ Right $ Op_RShiftE      }
    ">>>="          { \p _ -> L (mkPos p) $ Right $ Op_RRShiftE     }
    "@"             { \p _ -> L (mkPos p) $ Right $ Op_AtSign       }


{

type GenTokResult a = L (Either String a)
type TokResult = GenTokResult Token

dropDoubleSuffix :: String -> String
dropDoubleSuffix s =
  case reverse s of
    'd':rest -> reverse rest
    'D':rest -> reverse rest
    _ -> s

removeUnder :: String -> String
removeUnder = filter (/= '_')

parseNum :: (Eq a, Num a) => AlexPosn -> (String -> [(a, String)]) -> String -> (a -> b) -> GenTokResult b
parseNum p f s mkTok =
  case f (removeUnder s) of
    [(x, "")] -> L (mkPos p) (Right (mkTok x))
    _ -> lexicalError p $ "invalid numeric literal: " ++ s

readHexExp :: (Floating a, Eq a) => AlexPosn -> String -> (a -> Token) -> TokResult
readHexExp p initial mkTok =
    case readHex (removeUnder initial) of
      [(m, suf)] ->
          case suf of
            q:s | toLower q == 'p' ->
                case parseNum p readHex s id of
                  L _ (Right n) -> L (mkPos p) (Right (mkTok (m ** n)))
                  _  -> lexicalError p $ "invalid numeric literal: " ++ initial
            _  -> lexicalError p $ "invalid numeric literal: " ++ initial
      _ -> lexicalError p $ "invalid numeric literal: " ++ initial

readCharTok :: AlexPosn -> String -> TokResult
readCharTok p s =
    L (mkPos p) $
    case convChar (dropQuotes s) of
      Left err -> Left err
      Right (c:_) -> Right (CharTok c)
      Right [] -> Left ("invalid char literal " ++ s)

readStringTok :: AlexPosn -> String -> TokResult
readStringTok p s =
  L (mkPos p) $
  case convChar (dropQuotes s) of
    Left err -> Left err
    Right s -> Right (StringTok s)

dropQuotes :: String -> String
dropQuotes s = take (length s - 2) (tail s)

-- Converts a sequence of (unquoted) Java character literals, including
-- escapes, into the sequence of corresponding Chars. The calls to
-- 'lexicalError' double-check that this function is consistent with
-- the lexer rules for character and string literals. This function
-- could be expressed as another Alex lexer, but it's simple enough
-- to implement by hand.
convChar :: String -> Either String String
convChar s = loop s ""
  where
    loop ('\\':'u':s@(d1:d2:d3:d4:s')) acc =
      -- TODO: this is the wrong place for handling unicode escapes
      -- according to the Java Language Specification. Unicode escapes can
      -- appear anywhere in the source text, and are best processed
      -- before lexing.
      if all isHexDigit [d1,d2,d3,d4]
      then loop s' (toEnum (read ['0','x',d1,d2,d3,d4]) : acc)
      else Left $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
    loop ('\\':'u':s) _ =
      Left $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
    loop ('\\':c:s) acc =
      if isOctDigit c
      then convOctal maxRemainingOctals
      else case c of
             'b' -> loop s ('\b' : acc)
             'f' -> loop s ('\f' : acc)
             'n' -> loop s ('\n' : acc)
             'r' -> loop s ('\r' : acc)
             't' -> loop s ('\t' : acc)
             '\'' -> loop s ('\'' : acc)
             '\\' -> loop s ('\\' : acc)
             '"' -> loop s ('"' : acc)
             _ -> Left $ "bad escape \"\\" ++ c:"\""
      where maxRemainingOctals =
              if c <= '3' then 2 else 1
            convOctal n =
              let octals = takeWhile isOctDigit $ take n s
                  noctals = length octals
                  toChar = toEnum . fst . head . readOct
              in loop (drop noctals s) (toChar (c:octals) : acc)
    loop ("\\") _ =
      Left ("bad escape \"\\\"")
    loop (x:s) acc = loop s (x : acc)
    loop "" acc = Right (reverse acc)

lexicalError :: AlexPosn -> String -> GenTokResult a
lexicalError p s = L (mkPos p) (Left ("lexical error: " ++ s))

data L a = L Pos a
  deriving (Show, Eq)

-- (line, column)
type Pos = (Int, Int)

mkPos :: AlexPosn -> Pos
mkPos (AlexPn _ l c) = (l,c)

data Token
    -- Keywords
    = KW_Abstract
    | KW_AnnInterface
    | KW_Assert
    | KW_Boolean
    | KW_Break
    | KW_Byte
    | KW_Case
    | KW_Catch
    | KW_Char
    | KW_Class
    | KW_Const
    | KW_Continue
    | KW_Default
    | KW_Do
    | KW_Double
    | KW_Else
    | KW_Enum
    | KW_Extends
    | KW_Final
    | KW_Finally
    | KW_Float
    | KW_For
    | KW_Goto
    | KW_If
    | KW_Implements
    | KW_Import
    | KW_Instanceof
    | KW_Int
    | KW_Interface
    | KW_Long
    | KW_Native
    | KW_New
    | KW_Package
    | KW_Private
    | KW_Protected
    | KW_Public
    | KW_Record
    | KW_Return
    | KW_Short
    | KW_Static
    | KW_Strictfp
    | KW_Super
    | KW_Switch
    | KW_Synchronized
    | KW_This
    | KW_Throw
    | KW_Throws
    | KW_Transient
    | KW_Try
    | KW_Void
    | KW_Volatile
    | KW_While

    -- Separators
    | OpenParen
    | CloseParen
    | OpenSquare
    | CloseSquare
    | OpenCurly
    | CloseCurly
    | SemiColon
    | Comma
    | Period
    | LambdaArrow
    | MethodRefSep

    -- Literals
    | IntTok  Integer
    | LongTok Integer
    | DoubleTok Double
    | FloatTok Double
    | CharTok Char
    | StringTok String
    | BoolTok Bool
    | NullTok

    -- Identifiers
    | IdentTok String

    -- Operators
    | Op_Equal
    | Op_GThan
    | Op_LThan
    | Op_Bang
    | Op_Tilde
    | Op_Query
    | Op_Colon
    | Op_Equals
    | Op_LThanE
    | Op_GThanE
    | Op_BangE
    | Op_AAnd
    | Op_OOr
    | Op_PPlus
    | Op_MMinus
    | Op_Plus
    | Op_Minus
    | Op_Star
    | Op_Slash
    | Op_And
    | Op_Or
    | Op_Caret
    | Op_Percent
    | Op_LShift
    | Op_PlusE
    | Op_MinusE
    | Op_StarE
    | Op_SlashE
    | Op_AndE
    | Op_OrE
    | Op_CaretE
    | Op_PercentE
    | Op_LShiftE
    | Op_RShiftE
    | Op_RRShiftE
    | Op_AtSign
  deriving (Show, Eq)

lexer :: String -> Either (Int, Int, String) [L Token]
lexer str = go (alexStartPos, '\n', [], str) []
  where
    go :: AlexInput -> [L Token] -> Either (Int, Int, String) [L Token]
    go inp@(p, _, _, str) acc =
      case alexScan inp 0 of
        AlexEOF -> Right (reverse acc)
        AlexSkip inp' len -> go inp' acc
        AlexToken inp' len act ->
            case act p (take len str) of
              L (line, col) (Left err) -> Left (line, col, err)
              L pos (Right x) -> go inp' (L pos x : acc)
        AlexError (p, _, _, _) ->
          let (line, col) = mkPos p
          in Left (line, col, "lexical error")

}
