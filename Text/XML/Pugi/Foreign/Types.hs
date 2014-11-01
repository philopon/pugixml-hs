{-# LANGUAGE ForeignFunctionInterface #-}
module Text.XML.Pugi.Foreign.Types where

import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Data.ByteString as S

newtype Document    = Document    (ForeignPtr Document)
newtype Node        = Node        (ForeignPtr Node)
newtype ParseResult = ParseResult (Ptr ParseResult)
newtype XPath rt = XPath (ForeignPtr (XPath rt))
data NodeSet  = NodeSet Int (ForeignPtr NodeSet)

data Attr
data XNode
type Attribute = (S.ByteString, S.ByteString)
type XPathNode = Either Node Attribute
