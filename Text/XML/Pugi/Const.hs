module Text.XML.Pugi.Const
    ( -- * NodeType
      NodeType
    , nodeTypeNull, nodeTypeDocument, nodeTypeElement, nodeTypePCData, nodeTypeCData
    , nodeTypeComment, nodeTypePi, nodeTypeDeclaration, nodeTypeDoctype

    -- * ParseStatus
    , ParseStatus
    , parseFileNotFound, parseIoError, parseOutOfMemory, parseInternalError
    , parseUnrecognizedTag, parseBadPi, parseBadComment, parseBadCData
    , parseBadDoctype, parseBadPCData, parseBadStartElement, parseBadAttribute
    , parseBadEndElement, parseEndElementMismatch, parseAppendInvalidRoot
    , parseNoDocumentElement

    -- * Encoding
    , Encoding
    , encodingAuto, encodingUtf8, encodingUtf16be, encodingUtf16le, encodingUtf16
    , encodingUtf32be, encodingUtf32le, encodingUtf32, encodingWChar, encodingLatin1

    -- * FormatFlags
    , FormatFlags
    , formatDefault, formatIndent, formatNoDeclaration, formatNoEscapes
    , formatRaw, formatSaveFileText, formatWriteBom

    -- * ParseFlags
    , ParseFlags
    , parseCData, parseComments, parseDeclaration, parseDefault, parseDoctype
    , parseEol, parseEscapes, parseFragment, parseFull, parseMinimal, parsePi
    , parseTrimPCData, parseWsPCData, parseWsPCDataSingle, parseWconvAttribute
    , parseWnormAttribute
    ) where

import Text.XML.Pugi.Foreign.Const
