{-# LANGUAGE ForeignFunctionInterface #-}
module Text.XML.Pugi.Foreign.XPath.Node where

import Control.Applicative
import Control.Exception

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal

import Text.XML.Pugi.Foreign.Types
import Text.XML.Pugi.Foreign.Node
import Text.XML.Pugi.Foreign.Attr

-- xpath_node
foreign import ccall unsafe delete_xpath_node :: Ptr XNode -> IO ()
foreign import ccall unsafe xpath_node_node :: Ptr XNode -> IO (Ptr Node)
foreign import ccall unsafe xpath_node_attribute :: Ptr XNode -> IO (Ptr Attr)

peekXNode :: Ptr XNode -> IO XPathNode
peekXNode p = do
    n <- xpath_node_node p
    if n == nullPtr
        then bracket (xpath_node_attribute p) delete_attr $ \a -> do
            an <- attrName a
            av <- attrValue a
            return $ Right (an, av)
        else Left . Node <$> newForeignPtr finalizerNode n


foreign import ccall unsafe "&delete_xpath_node_set" finalizerXpathNodeSet :: FinalizerPtr NodeSet

foreign import ccall unsafe xpath_node_set_size :: Ptr NodeSet -> IO CSize 
foreign import ccall unsafe xpath_node_set_empty :: Ptr NodeSet -> IO CInt
foreign import ccall unsafe xpath_node_set_index :: Ptr NodeSet -> CSize -> IO (Ptr XNode)

foreign import ccall unsafe "wrapper" wrap_xpath_node_mapper :: (Ptr XNode -> IO ()) -> IO (FunPtr (Ptr XNode -> IO ()))
foreign import ccall        xpath_node_set_map :: Ptr NodeSet -> FunPtr (Ptr XNode -> IO ()) -> IO ()

nodeSetSize :: NodeSet -> Int
nodeSetSize (NodeSet l _) = l

nodeSetEmpty :: NodeSet -> IO Bool
nodeSetEmpty (NodeSet _ fp) = toBool <$>
    withForeignPtr fp xpath_node_set_empty

nodeSetIndex :: NodeSet -> Int -> IO XPathNode
nodeSetIndex (NodeSet l fp) i
    | l > i = withForeignPtr fp $ \p ->
        bracket (xpath_node_set_index p (fromIntegral i))
        delete_xpath_node peekXNode
    | otherwise = fail "out of range"

nodeSetMapM_ :: (XPathNode -> IO ()) -> NodeSet -> IO ()
nodeSetMapM_ f (NodeSet _ fp) = withForeignPtr fp $ \p -> do
    let func x = peekXNode x >>= f
    bracket (wrap_xpath_node_mapper func) freeHaskellFunPtr $ \fn ->
        xpath_node_set_map p fn
