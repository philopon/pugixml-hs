module Text.XML.Pugi.Foreign.Types where

import Foreign.ForeignPtr
import Foreign.Ptr

newtype Document    = Document    (ForeignPtr Document) deriving Show
newtype Node        = Node        (ForeignPtr Node)     deriving Show
newtype ParseResult = ParseResult (Ptr ParseResult)     deriving Show
data Attr
