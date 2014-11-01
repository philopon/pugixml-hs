{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.XML.Pugi.Foreign.Const where

import Foreign.C

import Data.Default.Class
import Data.Bits
import Data.List
import Data.Monoid

foreign import ccall unsafe pugixml_version :: CInt

newtype NodeType = NodeType CInt deriving Eq
foreign import ccall unsafe "enum_node_type_null"        nodeTypeNull        :: NodeType
foreign import ccall unsafe "enum_node_type_document"    nodeTypeDocument    :: NodeType
foreign import ccall unsafe "enum_node_type_element"     nodeTypeElement     :: NodeType
foreign import ccall unsafe "enum_node_type_pcdata"      nodeTypePCData      :: NodeType
foreign import ccall unsafe "enum_node_type_cdata"       nodeTypeCData       :: NodeType
foreign import ccall unsafe "enum_node_type_comment"     nodeTypeComment     :: NodeType
foreign import ccall unsafe "enum_node_type_pi"          nodeTypePi          :: NodeType
foreign import ccall unsafe "enum_node_type_declaration" nodeTypeDeclaration :: NodeType
foreign import ccall unsafe "enum_node_type_doctype"     nodeTypeDoctype     :: NodeType

instance Show NodeType where
    show n
        | n == nodeTypeNull        = "nodeTypeNull"
        | n == nodeTypeDocument    = "nodeTypeDocument"
        | n == nodeTypeElement     = "nodeTypeElement"
        | n == nodeTypePCData      = "nodeTypePCData"
        | n == nodeTypeCData       = "nodeTypeCData"
        | n == nodeTypeComment     = "nodeTypeComment"
        | n == nodeTypePi          = "nodeTypePi"
        | n == nodeTypeDeclaration = "nodeTypeDeclaration"
        | n == nodeTypeDoctype     = "nodeTypeDoctype"
        | otherwise                = let NodeType i = n in "NodeType " ++ show i

newtype ParseStatus = ParseStatus CInt deriving Eq
foreign import ccall unsafe "enum_parse_status_ok"                   parseOk                 :: ParseStatus
foreign import ccall unsafe "enum_parse_status_file_not_found"       parseFileNotFound       :: ParseStatus
foreign import ccall unsafe "enum_parse_status_io_error"             parseIoError            :: ParseStatus
foreign import ccall unsafe "enum_parse_status_out_of_memory"        parseOutOfMemory        :: ParseStatus
foreign import ccall unsafe "enum_parse_status_internal_error"       parseInternalError      :: ParseStatus
foreign import ccall unsafe "enum_parse_status_unrecognized_tag"     parseUnrecognizedTag    :: ParseStatus
foreign import ccall unsafe "enum_parse_status_bad_pi"               parseBadPi              :: ParseStatus
foreign import ccall unsafe "enum_parse_status_bad_comment"          parseBadComment         :: ParseStatus
foreign import ccall unsafe "enum_parse_status_bad_cdata"            parseBadCData           :: ParseStatus
foreign import ccall unsafe "enum_parse_status_bad_doctype"          parseBadDoctype         :: ParseStatus
foreign import ccall unsafe "enum_parse_status_bad_pcdata"           parseBadPCData          :: ParseStatus
foreign import ccall unsafe "enum_parse_status_bad_start_element"    parseBadStartElement    :: ParseStatus
foreign import ccall unsafe "enum_parse_status_bad_attribute"        parseBadAttribute       :: ParseStatus
foreign import ccall unsafe "enum_parse_status_bad_end_element"      parseBadEndElement      :: ParseStatus
foreign import ccall unsafe "enum_parse_status_end_element_mismatch" parseEndElementMismatch :: ParseStatus
foreign import ccall unsafe "enum_parse_status_append_invalid_root"  parseAppendInvalidRoot  :: ParseStatus
foreign import ccall unsafe "enum_parse_status_no_document_element"  parseNoDocumentElement  :: ParseStatus

instance Show ParseStatus where
    show n
        | n == parseOk                 = "parseOk"
        | n == parseFileNotFound       = "parseFileNotFound"
        | n == parseIoError            = "parseIoError"
        | n == parseOutOfMemory        = "parseOutOfMemory"
        | n == parseInternalError      = "parseInternalError"
        | n == parseUnrecognizedTag    = "parseUnrecognizedTag"
        | n == parseBadPi              = "parseBadPi"
        | n == parseBadComment         = "parseBadComment"
        | n == parseBadCData           = "parseBadCData"
        | n == parseBadDoctype         = "parseBadDoctype"
        | n == parseBadPCData          = "parseBadPCData"
        | n == parseBadStartElement    = "parseBadStartElement"
        | n == parseBadAttribute       = "parseBadAttribute"
        | n == parseBadEndElement      = "parseBadEndElement"
        | n == parseEndElementMismatch = "parseEndElementMismatch"
        | n == parseAppendInvalidRoot  = "parseAppendInvalidRoot"
        | n == parseNoDocumentElement  = "parseNoDocumentElement"
        | otherwise                    = let ParseStatus i = n in "ParseStatus " ++ show i

newtype Encoding = Encoding CInt deriving Eq
foreign import ccall unsafe "enum_encoding_auto"     encodingAuto    :: Encoding
foreign import ccall unsafe "enum_encoding_utf8"     encodingUtf8    :: Encoding
foreign import ccall unsafe "enum_encoding_utf16_be" encodingUtf16be :: Encoding
foreign import ccall unsafe "enum_encoding_utf16_le" encodingUtf16le :: Encoding
foreign import ccall unsafe "enum_encoding_utf16"    encodingUtf16   :: Encoding
foreign import ccall unsafe "enum_encoding_utf32_be" encodingUtf32be :: Encoding
foreign import ccall unsafe "enum_encoding_utf32_le" encodingUtf32le :: Encoding
foreign import ccall unsafe "enum_encoding_utf32"    encodingUtf32   :: Encoding
foreign import ccall unsafe "enum_encoding_wchar"    encodingWChar   :: Encoding
foreign import ccall unsafe "enum_encoding_latin1"   encodingLatin1  :: Encoding

instance Show Encoding where
    show n
        | n == encodingAuto    = "encodingAuto"
        | n == encodingUtf8    = "encodingUtf8"
        | n == encodingUtf16be = "encodingUtf16be"
        | n == encodingUtf16le = "encodingUtf16le"
        | n == encodingUtf16   = "encodingUtf16"
        | n == encodingUtf32be = "encodingUtf32be"
        | n == encodingUtf32le = "encodingUtf32le"
        | n == encodingUtf32   = "encodingUtf32"
        | n == encodingWChar   = "encodingWChar"
        | n == encodingLatin1  = "encodingLatin1"
        | otherwise            = let Encoding i = n in "Encoding " ++ show i

instance Default Encoding where
    def = encodingAuto

newtype XPathType = XPathType CInt deriving (Show, Eq)
foreign import ccall unsafe "enum_xpath_value_type_none"     xPathTypeNode    :: XPathType
foreign import ccall unsafe "enum_xpath_value_type_node_set" xPathTypeNodeSet :: XPathType
foreign import ccall unsafe "enum_xpath_value_type_number"   xPathTypeNumber  :: XPathType
foreign import ccall unsafe "enum_xpath_value_type_string"   xPathString      :: XPathType
foreign import ccall unsafe "enum_xpath_value_type_boolean"  xPathBoolean     :: XPathType

newtype FormatFlags = FormatFlags CUInt deriving (Eq, Bits)
foreign import ccall unsafe "format_flag_default"        formatDefault       :: FormatFlags
foreign import ccall unsafe "format_flag_indent"         formatIndent        :: FormatFlags
foreign import ccall unsafe "format_flag_no_declaration" formatNoDeclaration :: FormatFlags
foreign import ccall unsafe "format_flag_no_escapes"     formatNoEscapes     :: FormatFlags
foreign import ccall unsafe "format_flag_raw"            formatRaw           :: FormatFlags
foreign import ccall unsafe "format_flag_save_file_text" formatSaveFileText  :: FormatFlags
foreign import ccall unsafe "format_flag_write_bom"      formatWriteBom      :: FormatFlags

formatFlags :: [(String, CUInt)]
formatFlags = [ ("Indent",        un formatIndent)
              , ("NoDeclaration", un formatNoDeclaration)
              , ("NoEscapes",     un formatNoEscapes)
              , ("Raw",           un formatRaw)
              , ("SaveFileText",  un formatSaveFileText)
              , ("WriteBom",      un formatWriteBom)
              ]
  where un (FormatFlags i) = i

instance Show FormatFlags where
    show (FormatFlags fs) = 
        let fn = foldl' (\f (n, i) -> if i .&. fs /= 0 then (f . (n:)) else f) id formatFlags
        in "FormatFlags(" ++ intercalate "|" (fn []) ++ ")"

instance Default FormatFlags where
    def = formatDefault

instance Monoid FormatFlags where
    mempty  = FormatFlags 0
    mappend = (.|.)

newtype ParseFlags = ParseFlags CUInt deriving (Eq, Bits)
foreign import ccall unsafe "parse_flag_cdata"            parseCData          :: ParseFlags 
foreign import ccall unsafe "parse_flag_comments"         parseComments       :: ParseFlags 
foreign import ccall unsafe "parse_flag_declaration"      parseDeclaration    :: ParseFlags 
foreign import ccall unsafe "parse_flag_default"          parseDefault        :: ParseFlags 
foreign import ccall unsafe "parse_flag_doctype"          parseDoctype        :: ParseFlags 
foreign import ccall unsafe "parse_flag_eol"              parseEol            :: ParseFlags 
foreign import ccall unsafe "parse_flag_escapes"          parseEscapes        :: ParseFlags 
foreign import ccall unsafe "parse_flag_fragment"         parseFragment       :: ParseFlags 
foreign import ccall unsafe "parse_flag_full"             parseFull           :: ParseFlags 
foreign import ccall unsafe "parse_flag_minimal"          parseMinimal        :: ParseFlags 
foreign import ccall unsafe "parse_flag_pi"               parsePi             :: ParseFlags 
foreign import ccall unsafe "parse_flag_trim_pcdata"      parseTrimPCData     :: ParseFlags
foreign import ccall unsafe "parse_flag_ws_pcdata"        parseWsPCData       :: ParseFlags
foreign import ccall unsafe "parse_flag_ws_pcdata_single" parseWsPCDataSingle :: ParseFlags
foreign import ccall unsafe "parse_flag_wconv_attribute"  parseWconvAttribute :: ParseFlags
foreign import ccall unsafe "parse_flag_wnorm_attribute"  parseWnormAttribute :: ParseFlags

parseFlags :: [(String, CUInt)]
parseFlags = [ ("CData"         , un parseCData)
             , ("Comments"      , un parseComments)
             , ("Declaration"   , un parseDeclaration)
             , ("Doctype"       , un parseDoctype)
             , ("Eol"           , un parseEol)
             , ("Escapes"       , un parseEscapes)
             , ("Fragment"      , un parseFragment)
             , ("Pi"            , un parsePi)
             , ("TrimPCData"    , un parseTrimPCData)
             , ("WsPCData"      , un parseWsPCData)
             , ("WsPCDataSingle", un parseWsPCDataSingle)
             , ("WconvAttribute", un parseWconvAttribute)
             , ("WnormAttribute", un parseWnormAttribute)
             ]
  where un (ParseFlags i) = i

instance Show ParseFlags where
    show (ParseFlags fs) = 
        let fn = foldl' (\f (n, i) -> if i .&. fs /= 0 then (f . (n:)) else f) id parseFlags
        in "ParseFlags(" ++ intercalate "|" (fn []) ++ ")"

instance Default ParseFlags where
    def = parseDefault

instance Monoid ParseFlags where
    mempty  = ParseFlags 0
    mappend = (.|.)
