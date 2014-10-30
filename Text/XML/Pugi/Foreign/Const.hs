module Text.XML.Pugi.Foreign.Const where

import Foreign.C

foreign import ccall unsafe pugixml_version :: CInt

newtype NodeType = NodeType CInt deriving (Show, Eq)
foreign import ccall unsafe "enum_node_type_null"        nodeTypeNull        :: NodeType
foreign import ccall unsafe "enum_node_type_document"    nodeTypeDocument    :: NodeType
foreign import ccall unsafe "enum_node_type_element"     nodeTypeElement     :: NodeType
foreign import ccall unsafe "enum_node_type_pcdata"      nodeTypePCData      :: NodeType
foreign import ccall unsafe "enum_node_type_cdata"       nodeTypeCData       :: NodeType
foreign import ccall unsafe "enum_node_type_comment"     nodeTypeComment     :: NodeType
foreign import ccall unsafe "enum_node_type_pi"          nodeTypePi          :: NodeType
foreign import ccall unsafe "enum_node_type_declaration" nodeTypeDeclaration :: NodeType
foreign import ccall unsafe "enum_node_type_doctype"     nodeTypeDoctype     :: NodeType

newtype ParseStatus = ParseStatus CInt deriving (Show, Eq)
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

newtype Encoding = Encoding CInt deriving (Show, Eq)
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

newtype XPathType = XPathType CInt deriving (Show, Eq)
foreign import ccall unsafe "enum_xpath_value_type_none"     xPathTypeNode    :: XPathType
foreign import ccall unsafe "enum_xpath_value_type_node_set" xPathTypeNodeSet :: XPathType
foreign import ccall unsafe "enum_xpath_value_type_number"   xPathTypeNumber  :: XPathType
foreign import ccall unsafe "enum_xpath_value_type_string"   xPathString      :: XPathType
foreign import ccall unsafe "enum_xpath_value_type_boolean"  xPathBoolean     :: XPathType

newtype FormatFlags = FormatFlags CUInt deriving (Show, Eq)
foreign import ccall unsafe "format_flag_default"        formatFlagDefault       :: FormatFlags
foreign import ccall unsafe "format_flag_indent"         formatFlagIndent        :: FormatFlags
foreign import ccall unsafe "format_flag_no_declaration" formatFlagNoDeclaration :: FormatFlags
foreign import ccall unsafe "format_flag_no_escapes"     formatFlagNoEscapes     :: FormatFlags
foreign import ccall unsafe "format_flag_raw"            formatFlagRaw           :: FormatFlags
foreign import ccall unsafe "format_flag_save_file_text" formatFlagSaveFileText  :: FormatFlags
foreign import ccall unsafe "format_flag_write_bom"      formatFlagWriteBom      :: FormatFlags

newtype ParseFlags = ParseFlags CUInt deriving (Show, Eq)
foreign import ccall unsafe "parse_flag_cdata"            parseFlagCData          :: ParseFlags 
foreign import ccall unsafe "parse_flag_comments"         parseFlagComments       :: ParseFlags 
foreign import ccall unsafe "parse_flag_declaration"      parseFlagDeclaration    :: ParseFlags 
foreign import ccall unsafe "parse_flag_default"          parseFlagDefault        :: ParseFlags 
foreign import ccall unsafe "parse_flag_doctype"          parseFlagDoctype        :: ParseFlags 
foreign import ccall unsafe "parse_flag_eol"              parseFlagEol            :: ParseFlags 
foreign import ccall unsafe "parse_flag_escapes"          parseFlagEscapes        :: ParseFlags 
foreign import ccall unsafe "parse_flag_fragment"         parseFlagFragment       :: ParseFlags 
foreign import ccall unsafe "parse_flag_full"             parseFlagFull           :: ParseFlags 
foreign import ccall unsafe "parse_flag_minimal"          parseFlagMinimal        :: ParseFlags 
foreign import ccall unsafe "parse_flag_pi"               parseFlagPi             :: ParseFlags 
foreign import ccall unsafe "parse_flag_trim_pcdata"      parseFlagTrimPCData     :: ParseFlags
foreign import ccall unsafe "parse_flag_ws_pcdata"        parseFlagWsPCData       :: ParseFlags
foreign import ccall unsafe "parse_flag_ws_pcdata_single" parseFlagWsPCDataSingle :: ParseFlags
foreign import ccall unsafe "parse_flag_wconv_attribute"  parseFlagWconvAttribute :: ParseFlags
foreign import ccall unsafe "parse_flag_wnorm_attribute"  parseFlagWnormAttribute :: ParseFlags
