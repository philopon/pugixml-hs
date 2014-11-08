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

import Data.IORef

foreign import ccall xpath_node_set_empty :: Ptr (NodeSet m) -> IO CInt
foreign import ccall xpath_node_set_index :: Ptr (NodeSet m) -> CSize -> IO (Ptr XNode)

foreign import ccall "wrapper" wrap_xpath_node_mapper :: (Ptr XNode -> IO ()) -> IO (FunPtr (Ptr XNode -> IO ()))
foreign import ccall xpath_node_set_map :: Ptr (NodeSet m) -> FunPtr (Ptr XNode -> IO ()) -> IO ()

nodeSetSize :: NodeSet m -> Int
nodeSetSize (NodeSet l _) = l

nodeSetEmpty :: NodeSet m -> IO Bool
nodeSetEmpty (NodeSet _ fp) = toBool <$>
    withForeignPtr fp xpath_node_set_empty

nodeSetIndex :: NodeSet m -> Int -> IO (XPathNode m)
nodeSetIndex (NodeSet l fp) i
    | l > i = withForeignPtr fp $ \p ->
        bracket (xpath_node_set_index p (fromIntegral i))
        delete_xpath_node peekXNode
    | otherwise = fail "out of range"

nodeSetMapM_ :: (XPathNode m -> IO ()) -> NodeSet m -> IO ()
nodeSetMapM_ f (NodeSet _ fp) = withForeignPtr fp $ \p -> do
    let func x = peekXNode x >>= f
    bracket (wrap_xpath_node_mapper func) freeHaskellFunPtr $ \fn ->
        xpath_node_set_map p fn

nodeSetMapM :: (XPathNode m -> IO a) -> NodeSet m -> IO [a]
nodeSetMapM f n = do
    ref <- newIORef id
    nodeSetMapM_ (\x -> f x >>= \a -> modifyIORef ref (\rf -> rf . (a:))) n
    readIORef ref >>= \l -> return (l [])
