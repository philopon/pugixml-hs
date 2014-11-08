{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.XML.Pugi.Foreign.XPath where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Control.Applicative
import Control.Exception

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal
import qualified Data.ByteString as S
import Data.String(IsString(..))
import System.IO.Unsafe

import Text.XML.Pugi.Foreign.Types
import Text.XML.Pugi.Foreign.Const
import Text.XML.Pugi.Foreign.Node

foreign import ccall delete_xpath_query :: Ptr (XPath a) -> IO ()
foreign import ccall "&delete_xpath_query" finalizerXpathQuery :: FinalizerPtr (XPath a)
foreign import ccall new_xpath_query_no_variable :: CString -> IO (Ptr (XPath a))

foreign import ccall xpath_query_evaluate_boolean  :: Ptr (XPath Bool)         -> Ptr n -> IO CInt
foreign import ccall xpath_query_evaluate_number   :: Ptr (XPath Double)       -> Ptr n -> IO CDouble
foreign import ccall xpath_query_evaluate_string   :: Ptr (XPath S.ByteString) -> Ptr n -> IO CString
foreign import ccall xpath_query_evaluate_node_set :: Ptr (XPath (NodeSet m))  -> Ptr n -> IO (Ptr (NodeSet m))

foreign import ccall xpath_query_return_type :: Ptr (XPath a) -> IO XPathType
foreign import ccall xpath_query_parse_is_success :: Ptr (XPath a) -> IO CInt

createXPath :: S.ByteString -> IO (XPath a)
createXPath query = S.useAsCString query $ \c -> do
    p <- new_xpath_query_no_variable c
    XPath <$> newForeignPtr finalizerXpathQuery p

createXPath' :: String -> XPath a
createXPath' q = unsafeDupablePerformIO $ createXPath (fromString q)

asNodeSet :: XPath (NodeSet m) -> XPath (NodeSet m)
asNodeSet = id

asDouble :: XPath Double -> XPath Double
asDouble = id

asByteString :: XPath S.ByteString -> XPath S.ByteString
asByteString = id

asBool :: XPath Bool -> XPath Bool
asBool = id

xpath' :: String -> ExpQ
xpath' str = do
    rt <- runIO $ withCString str $ \c ->
        bracket (new_xpath_query_no_variable c) delete_xpath_query $ \x ->
        (toBool <$> xpath_query_parse_is_success x) >>= \case
            False -> fail $ "xpath compile failed: " ++ show str
            True  -> xpath_query_return_type x
    let as = if
            | rt == xPathTypeNodeSet -> [|asNodeSet|]
            | rt == xPathTypeNumber  -> [|asDouble|]
            | rt == xPathString      -> [|asByteString|]
            | rt == xPathBoolean     -> [|asBool|]
            | otherwise              -> fail $ "xpath_type_none"
    [|$as (createXPath' $(stringE str))|]

-- | generate xpath object.
--
-- @
-- [xpath|query|] == ((xpathObject) :: XPath (instance of EvalXPath))
-- @
--
xpath :: QuasiQuoter
xpath = QuasiQuoter 
    { quoteExp  = xpath'
    , quotePat  = error "xpath QQ can use as Exp only."
    , quoteType = error "xpath QQ can use as Exp only."
    , quoteDec  = error "xpath QQ can use as Exp only."
    }

class EvalXPath a where
    evaluateXPath :: NodeLike n => XPath a -> n k m -> IO a

instance EvalXPath Bool where
    evaluateXPath (XPath xp) nd = withForeignPtr xp $ \x -> withNode nd $ \n ->
        toBool <$> xpath_query_evaluate_boolean x n

instance EvalXPath Double where
    evaluateXPath (XPath xp) nd = withForeignPtr xp $ \x -> withNode nd $ \n ->
        realToFrac <$> xpath_query_evaluate_number x n

instance EvalXPath S.ByteString where
    evaluateXPath (XPath xp) nd = withForeignPtr xp $ \x -> withNode nd $ \n -> do
        s <- xpath_query_evaluate_string x n
        S.packCString s <* free s

instance EvalXPath (NodeSet m) where
    evaluateXPath (XPath xp) nd = withForeignPtr xp $ \x -> withNode nd $ \n -> do
        s <- xpath_query_evaluate_node_set x n
        l <- fromIntegral <$> xpath_node_set_size s
        NodeSet l <$> newForeignPtr finalizerXpathNodeSet s
