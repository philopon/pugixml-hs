{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.XML.Pugi.Foreign.Node where

import Control.Applicative
import Control.Monad
import Control.Exception

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal

import Data.IORef
import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString as S

import Text.XML.Pugi.Foreign.Const
import Text.XML.Pugi.Foreign.Types
import Text.XML.Pugi.Foreign.Attr

-- node
foreign import ccall unsafe "&delete_node" finalizerNode
    :: FinalizerPtr Node

foreign import ccall unsafe node_hash_value :: Ptr n -> IO CSize

foreign import ccall unsafe node_type :: Ptr n -> IO NodeType

foreign import ccall unsafe node_name  :: Ptr n -> IO CString
foreign import ccall unsafe node_value :: Ptr n -> IO CString

foreign import ccall unsafe node_parent           :: Ptr n -> IO (Ptr Node)
foreign import ccall unsafe node_first_child      :: Ptr n -> IO (Ptr Node)
foreign import ccall unsafe node_last_child       :: Ptr n -> IO (Ptr Node)
foreign import ccall unsafe node_next_sibling     :: Ptr n -> IO (Ptr Node)
foreign import ccall unsafe node_previous_sibling :: Ptr n -> IO (Ptr Node)

foreign import ccall unsafe node_child :: Ptr n -> CString -> IO (Ptr Node)
foreign import ccall unsafe node_attribute :: Ptr n -> CString -> IO (Ptr Attr)
foreign import ccall unsafe node_next_sibling_by_name :: Ptr n -> CString -> IO (Ptr Node)
foreign import ccall unsafe node_previous_sibling_by_name :: Ptr n -> CString -> IO (Ptr Node)
foreign import ccall unsafe node_find_child_by_name_and_attribute :: Ptr n -> CString -> CString -> CString -> IO (Ptr Node)
foreign import ccall unsafe node_find_child_by_attribute :: Ptr n -> CString -> CString -> IO (Ptr Node)

foreign import ccall unsafe node_child_value :: Ptr n -> IO CString
foreign import ccall unsafe node_child_value_by_name :: Ptr n -> CString -> IO CString
foreign import ccall unsafe node_text :: Ptr n -> IO CString

type NodeMapper = Ptr Node -> IO ()
foreign import ccall unsafe "wrapper" wrap_node_mapper :: NodeMapper -> IO (FunPtr NodeMapper)
foreign import ccall node_map_sibling :: Ptr n -> FunPtr NodeMapper -> IO ()

type AttrMapper = Ptr Attr -> IO ()
foreign import ccall unsafe "wrapper" wrap_attr_mapper :: AttrMapper -> IO (FunPtr AttrMapper)
foreign import ccall node_map_attributes :: Ptr n -> FunPtr AttrMapper -> IO ()

foreign import ccall unsafe node_path :: Ptr n -> CChar -> IO CString -- must be free

foreign import ccall unsafe node_first_element_by_path :: Ptr n -> CString -> CChar -> IO (Ptr Node)

foreign import ccall unsafe node_root :: Ptr a -> IO (Ptr Node)

nodeCommon :: NodeLike n => n -> (Ptr n -> IO (Ptr Node)) -> IO (Maybe Node)
nodeCommon n f = withNode n $ \p -> do
    q <- f p
    if q == nullPtr
    then return Nothing
    else Just . Node <$> newForeignPtr finalizerNode q

class NodeLike n where
    withNode :: n -> (Ptr n -> IO a) -> IO a

    hashValue :: n -> IO CSize
    hashValue n = withNode n node_hash_value
    nodeType  :: n -> IO NodeType
    nodeType n = withNode n node_type

    getName  :: n -> IO S.ByteString
    getName n = withNode n $ node_name >=> S.packCString
    getValue :: n -> IO S.ByteString
    getValue n = withNode n $ node_value >=> S.packCString

    parent :: n -> IO (Maybe Node)
    parent n = nodeCommon n node_parent
    firstChild :: n -> IO (Maybe Node)
    firstChild n = nodeCommon n node_first_child
    lastChild :: n -> IO (Maybe Node)
    lastChild n = nodeCommon n node_last_child
    nextSibling :: n -> IO (Maybe Node)
    nextSibling n = nodeCommon n node_next_sibling
    prevSibling :: n -> IO (Maybe Node)
    prevSibling n = nodeCommon n node_previous_sibling

    child :: S.ByteString -> n -> IO (Maybe Node)
    child nm n = nodeCommon n $ \p -> S.useAsCString nm $ node_child p

    attribute :: S.ByteString -> n -> IO (Maybe S.ByteString)
    attribute nm n = withNode n $ \p -> S.useAsCString nm $ \a ->
        bracket (node_attribute p a) delete_attr $ \attr ->
        if attr == nullPtr
        then return Nothing
        else fmap Just $ attr_value attr >>= S.packCString

    nextSiblingByName :: S.ByteString -> n -> IO (Maybe Node)
    nextSiblingByName nm n = nodeCommon n $ \p -> S.useAsCString nm $ node_next_sibling_by_name p
    prevSiblingByName :: S.ByteString -> n -> IO (Maybe Node)
    prevSiblingByName nm n = nodeCommon n $ \p -> S.useAsCString nm $ node_previous_sibling_by_name p

    findChildByNameAndAttr :: S.ByteString -- ^ node name
                           -> S.ByteString -- ^ attribute name
                           -> S.ByteString -- ^ attribute value
                           -> n -> IO (Maybe Node)
    findChildByNameAndAttr nn an av n = nodeCommon n $ \p ->
        S.useAsCString nn $ \nn' -> S.useAsCString an $ \an' -> S.useAsCString av $ \av' ->
        node_find_child_by_name_and_attribute p nn' an' av'

    findChildByAttr :: S.ByteString -- ^ attribute name
                    -> S.ByteString -- ^ attribute value
                    -> n -> IO (Maybe Node)
    findChildByAttr an av n = nodeCommon n $ \p ->
        S.useAsCString an $ \an' -> S.useAsCString av $ \av' ->
        node_find_child_by_attribute p an' av'
    
    childValue :: n -> IO S.ByteString
    childValue n = withNode n $ node_child_value >=> S.packCString

    childValueByName :: S.ByteString -> n -> IO S.ByteString
    childValueByName nm n = withNode n $ \p -> S.useAsCString nm $ \s ->
        node_child_value_by_name p s >>= S.packCString

    text :: n -> IO S.ByteString
    text n = withNode n $ node_text >=> S.packCString

    mapSiblingM_ :: (Node -> IO ()) -> n -> IO ()
    mapSiblingM_ func n = withNode n $ \p -> do
        let f e = func . Node =<< newForeignPtr_ e
        bracket (wrap_node_mapper f) freeHaskellFunPtr $ node_map_sibling p

    mapAttrsM_ :: (S.ByteString -> S.ByteString -> IO ()) -> n -> IO ()
    mapAttrsM_ func n = withNode n $ \p -> do
        let f a = do
                nm  <- attrName a
                val <- attrValue a
                func nm val
        bracket (wrap_attr_mapper f) freeHaskellFunPtr $ node_map_attributes p

    path :: Char -> n -> IO S.ByteString
    path del node = withNode node $ \p ->
        bracket (node_path p (fromIntegral $ c2w del)) free $ S.packCString

    firstElementByPath :: Char -> S.ByteString -> n -> IO (Maybe Node)
    firstElementByPath del path_ node = nodeCommon node $ \p ->
        S.useAsCString path_ $ \d -> node_first_element_by_path p d (fromIntegral $ c2w del)

    root :: n -> IO (Maybe Node)
    root node = nodeCommon node $ node_root
        

mapSiblingM :: NodeLike n => (Node -> IO a) -> n -> IO [a]
mapSiblingM func node = do
    ref <- newIORef id
    mapSiblingM_ (\n -> func n >>= \a -> modifyIORef ref (\f -> f . (a:))) node
    readIORef ref >>= \a -> return (a [])

mapAttrsM :: NodeLike n => (S.ByteString -> S.ByteString -> IO a) -> n -> IO [a]
mapAttrsM func n = do
    ref <- newIORef id
    mapAttrsM_ (\k v -> func k v >>= \a -> modifyIORef ref (\f -> f . (a:))) n
    readIORef ref >>= \a -> return (a [])

instance NodeLike Document where withNode (Document f) = withForeignPtr f
instance NodeLike Node     where withNode (Node     f) = withForeignPtr f
