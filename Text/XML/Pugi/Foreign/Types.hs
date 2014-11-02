{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Text.XML.Pugi.Foreign.Types where

import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Data.ByteString as S

data MutableFlag = Mutable | Immutable

newtype Document_ (mutable :: MutableFlag) = Document (ForeignPtr (Document_ mutable))
type Document = Document_ Immutable
type MutableDocument = Document_ Mutable


newtype Node_ (mutable :: MutableFlag) = Node (ForeignPtr (Node_ mutable))
type Node = Node_ Immutable
type MutableNode = Node_ Mutable

newtype ParseResult = ParseResult (Ptr ParseResult)

newtype XPath rt = XPath (ForeignPtr (XPath rt))

data NodeSet  = NodeSet Int (ForeignPtr NodeSet)


data Attr
data XNode
type Attribute = (S.ByteString, S.ByteString)
type XPathNode = Either Node Attribute
