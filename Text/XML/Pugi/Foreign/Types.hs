{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Text.XML.Pugi.Foreign.Types where

import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Data.ByteString as S

data MutableFlag = Mutable | Immutable

data NodeKind = Element -- ^ \<name\>children\</name\>
              | PCData -- ^ value
              | CData -- ^ \<![CDATA[value]]\>
              | Comment -- ^ \<!--value--\>
              | Pi -- ^ \<?name value?>
              | Declaration -- ^ \<?name?\>
              | Doctype -- ^ \<!DOCTYPE value\>
              | Unknown

newtype Document_ (k :: NodeKind) (m :: MutableFlag) = Document (ForeignPtr (Document_ k m))
type Document = Document_ Unknown Immutable
type MutableDocument = Document_ Unknown Mutable

newtype Node_ (k :: NodeKind) (m :: MutableFlag) = Node (ForeignPtr (Node_ k m))
type Node = Node_ Unknown Immutable
type MutableNode k = Node_ k Mutable

newtype ParseResult = ParseResult (Ptr ParseResult)

newtype XPath rt = XPath (ForeignPtr (XPath rt))

data NodeSet (m :: MutableFlag) = NodeSet Int (ForeignPtr (NodeSet m))

data Attr
data XNode
type Attribute = (S.ByteString, S.ByteString)
type XPathNode m = Either (Node_ Unknown m) Attribute
