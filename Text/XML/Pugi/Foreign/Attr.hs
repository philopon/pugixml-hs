{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.XML.Pugi.Foreign.Attr where

import Control.Monad

import Foreign.C
import Foreign.Ptr

import qualified Data.ByteString as S

import Text.XML.Pugi.Foreign.Types

-- attr
foreign import ccall delete_attr :: Ptr Attr -> IO ()
foreign import ccall attr_name   :: Ptr Attr -> IO CString
foreign import ccall attr_value  :: Ptr Attr -> IO CString
foreign import ccall attr_set_value :: Ptr Attr -> CString -> IO CInt

attrName, attrValue :: Ptr Attr -> IO S.ByteString
attrName  = attr_name  >=> S.packCString
attrValue = attr_value >=> S.packCString
