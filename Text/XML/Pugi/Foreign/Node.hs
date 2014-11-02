{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
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
import qualified Data.ByteString.Unsafe as S

import Text.XML.Pugi.Foreign.Document
import Text.XML.Pugi.Foreign.Const
import Text.XML.Pugi.Foreign.Types
import Text.XML.Pugi.Foreign.Attr

-- node
foreign import ccall unsafe "&delete_node" finalizerNode
    :: FinalizerPtr (Node_ a)

foreign import ccall unsafe node_hash_value :: Ptr n -> IO CSize

foreign import ccall unsafe node_type :: Ptr n -> IO NodeType

foreign import ccall unsafe node_name  :: Ptr n -> IO CString
foreign import ccall unsafe node_value :: Ptr n -> IO CString

foreign import ccall unsafe node_parent           :: Ptr n -> IO (Ptr (Node_ a))
foreign import ccall unsafe node_first_child      :: Ptr n -> IO (Ptr (Node_ a))
foreign import ccall unsafe node_last_child       :: Ptr n -> IO (Ptr (Node_ a))
foreign import ccall unsafe node_next_sibling     :: Ptr n -> IO (Ptr (Node_ a))
foreign import ccall unsafe node_previous_sibling :: Ptr n -> IO (Ptr (Node_ a))

foreign import ccall unsafe node_child :: Ptr n -> CString -> IO (Ptr (Node_ a))
foreign import ccall unsafe node_attribute :: Ptr n -> CString -> IO (Ptr Attr)
foreign import ccall unsafe node_next_sibling_by_name :: Ptr n -> CString -> IO (Ptr (Node_ a))
foreign import ccall unsafe node_previous_sibling_by_name :: Ptr n -> CString -> IO (Ptr (Node_ a))
foreign import ccall unsafe node_find_child_by_name_and_attribute :: Ptr n -> CString -> CString -> CString -> IO (Ptr (Node_ a))
foreign import ccall unsafe node_find_child_by_attribute :: Ptr n -> CString -> CString -> IO (Ptr (Node_ a))

foreign import ccall unsafe node_child_value :: Ptr n -> IO CString
foreign import ccall unsafe node_child_value_by_name :: Ptr n -> CString -> IO CString
foreign import ccall unsafe node_text :: Ptr n -> IO CString

type NodeMapper a = Ptr (Node_ a) -> IO ()
foreign import ccall unsafe "wrapper" wrap_node_mapper :: NodeMapper a -> IO (FunPtr (NodeMapper a))
foreign import ccall node_map_sibling :: Ptr n -> FunPtr (NodeMapper a) -> IO ()

type AttrMapper = Ptr Attr -> IO ()
foreign import ccall unsafe "wrapper" wrap_attr_mapper :: AttrMapper -> IO (FunPtr AttrMapper)
foreign import ccall node_map_attributes :: Ptr n -> FunPtr AttrMapper -> IO ()

foreign import ccall unsafe node_path :: Ptr n -> CChar -> IO CString -- must be free

foreign import ccall unsafe node_first_element_by_path :: Ptr n -> CString -> CChar -> IO (Ptr (Node_ a))

foreign import ccall unsafe node_root :: Ptr n -> IO (Ptr (Node_ a))

foreign import ccall unsafe set_name  :: Ptr n -> IO CInt
foreign import ccall unsafe set_value :: Ptr n -> IO CInt

foreign import ccall unsafe append_attribute  :: Ptr n -> CString -> CString -> IO CInt
foreign import ccall unsafe prepend_attribute :: Ptr n -> CString -> CString -> IO CInt

foreign import ccall unsafe append_child  :: Ptr n -> NodeType -> IO (Ptr (Node_ a))
foreign import ccall unsafe prepend_child :: Ptr n -> NodeType -> IO (Ptr (Node_ a))

foreign import ccall unsafe append_copy  :: Ptr n -> Ptr (Node_ a) -> IO (Ptr (Node_ b))
foreign import ccall unsafe prepend_copy :: Ptr n -> Ptr (Node_ a) -> IO (Ptr (Node_ b))

foreign import ccall unsafe remove_attribute :: Ptr n -> CString -> IO CInt
foreign import ccall unsafe remove_child     :: Ptr n -> Ptr (Node_ a) -> IO CInt

foreign import ccall unsafe append_buffer :: Ptr n -> Ptr c -> CSize -> CUInt -> Encoding -> IO ParseResult

foreign import ccall unsafe print :: Ptr n -> FunPtr Writer -> CString -> CUInt -> Encoding -> CUInt -> IO ()

foreign import ccall unsafe select_single_node :: Ptr n -> Ptr (XPath NodeSet) -> IO (Ptr XNode)
foreign import ccall unsafe select_nodes :: Ptr n -> Ptr (XPath NodeSet) -> IO (Ptr NodeSet)

nodeCommon :: NodeLike n => n m -> (Ptr (n m) -> IO (Ptr (Node_ m))) -> IO (Maybe (Node_ m))
nodeCommon n f = withNode n $ \p -> do
    q <- f p
    if q == nullPtr
    then return Nothing
    else Just . Node <$> newForeignPtr finalizerNode q

class NodeLike (n :: MutableFlag -> *) where
    withNode :: n m -> (Ptr (n m) -> IO a) -> IO a

    hashValue :: n m -> IO CSize
    hashValue n = withNode n node_hash_value
    nodeType  :: n m -> IO NodeType
    nodeType n = withNode n node_type

    getName  :: n m -> IO S.ByteString
    getName n = withNode n $ node_name >=> S.packCString
    getValue :: n m -> IO S.ByteString
    getValue n = withNode n $ node_value >=> S.packCString

    parent :: n m -> IO (Maybe (Node_ m))
    parent n = nodeCommon n node_parent
    firstChild :: n m -> IO (Maybe (Node_ m))
    firstChild n = nodeCommon n node_first_child
    lastChild :: n m -> IO (Maybe (Node_ m))
    lastChild n = nodeCommon n node_last_child
    nextSibling :: n m -> IO (Maybe (Node_ m))
    nextSibling n = nodeCommon n node_next_sibling
    prevSibling :: n m -> IO (Maybe (Node_ m))
    prevSibling n = nodeCommon n node_previous_sibling

    child :: S.ByteString -> n m -> IO (Maybe (Node_ m))
    child nm n = nodeCommon n $ \p -> S.useAsCString nm $ node_child p

    attribute :: S.ByteString -> n m -> IO (Maybe S.ByteString)
    attribute nm n = withNode n $ \p -> S.useAsCString nm $ \a ->
        bracket (node_attribute p a) delete_attr $ \attr ->
        if attr == nullPtr
        then return Nothing
        else fmap Just $ attr_value attr >>= S.packCString

    nextSiblingByName :: S.ByteString -> n m -> IO (Maybe (Node_ m))
    nextSiblingByName nm n = nodeCommon n $ \p -> S.useAsCString nm $ node_next_sibling_by_name p
    prevSiblingByName :: S.ByteString -> n m -> IO (Maybe (Node_ m))
    prevSiblingByName nm n = nodeCommon n $ \p -> S.useAsCString nm $ node_previous_sibling_by_name p

    findChildByNameAndAttr :: S.ByteString -- ^ node name
                           -> S.ByteString -- ^ attribute name
                           -> S.ByteString -- ^ attribute value
                           -> n m -> IO (Maybe (Node_ m))
    findChildByNameAndAttr nn an av n = nodeCommon n $ \p ->
        S.useAsCString nn $ \nn' -> S.useAsCString an $ \an' -> S.useAsCString av $ \av' ->
        node_find_child_by_name_and_attribute p nn' an' av'

    findChildByAttr :: S.ByteString -- ^ attribute name
                    -> S.ByteString -- ^ attribute value
                    -> n m -> IO (Maybe (Node_ m))
    findChildByAttr an av n = nodeCommon n $ \p ->
        S.useAsCString an $ \an' -> S.useAsCString av $ \av' ->
        node_find_child_by_attribute p an' av'
    
    childValue :: n m -> IO S.ByteString
    childValue n = withNode n $ node_child_value >=> S.packCString

    childValueByName :: S.ByteString -> n m -> IO S.ByteString
    childValueByName nm n = withNode n $ \p -> S.useAsCString nm $ \s ->
        node_child_value_by_name p s >>= S.packCString

    text :: n m -> IO S.ByteString
    text n = withNode n $ node_text >=> S.packCString

    mapSiblingM_ :: (Node_ m -> IO ()) -> n m -> IO ()
    mapSiblingM_ func n = withNode n $ \p -> do
        let f e = func . Node =<< newForeignPtr_ e
        bracket (wrap_node_mapper f) freeHaskellFunPtr $ node_map_sibling p

    mapAttrsM_ :: (S.ByteString -> S.ByteString -> IO ()) -> n m -> IO ()
    mapAttrsM_ func n = withNode n $ \p -> do
        let f a = do
                nm  <- attrName a
                val <- attrValue a
                func nm val
        bracket (wrap_attr_mapper f) freeHaskellFunPtr $ node_map_attributes p

    path :: Char -> n m -> IO S.ByteString
    path del node = withNode node $ \p ->
        bracket (node_path p (fromIntegral $ c2w del)) free $ S.packCString

    firstElementByPath :: Char -> S.ByteString -> n m -> IO (Maybe (Node_ m))
    firstElementByPath del path_ node = nodeCommon node $ \p ->
        S.useAsCString path_ $ \d -> node_first_element_by_path p d (fromIntegral $ c2w del)

    root :: n m -> IO (Maybe (Node_ m))
    root node = nodeCommon node $ node_root

    setName :: n Mutable -> IO Bool
    setName node = toBool <$> withNode node set_name
    setValue :: n Mutable -> IO Bool
    setValue node = toBool <$> withNode node set_value

    appendAttr :: n Mutable -> S.ByteString -> S.ByteString -> IO Bool
    appendAttr n k v = withNode n $ \np -> S.useAsCString k $ \kp -> S.useAsCString v $ \vp ->
        toBool <$> append_attribute np kp vp
    prependAttr :: n Mutable -> S.ByteString -> S.ByteString -> IO Bool
    prependAttr n k v = withNode n $ \np -> S.useAsCString k $ \kp -> S.useAsCString v $ \vp ->
        toBool <$> prepend_attribute np kp vp

    appendChild :: n Mutable -> NodeType -> IO (Maybe MutableNode)
    appendChild n t = nodeCommon n $ \p -> append_child p t
    prependChild :: n Mutable -> NodeType -> IO (Maybe MutableNode)
    prependChild n t = nodeCommon n $ \p -> prepend_child p t

    appendCopy :: n Mutable -> Node_ a -> IO (Maybe MutableNode)
    appendCopy n t = nodeCommon n $ \p -> withNode t (append_copy p)
    prependCopy :: n Mutable -> Node_ a -> IO (Maybe MutableNode)
    prependCopy n t = nodeCommon n $ \p -> withNode t (prepend_copy p)

    removeAttr :: n Mutable -> S.ByteString -> IO Bool
    removeAttr n a = withNode n $ \p -> S.useAsCString a $ \c ->
        toBool <$> remove_attribute p c

    removeChild :: n Mutable -> Node_ a -> IO Bool
    removeChild n c = withNode n $ \p -> withNode c $ \q ->
        toBool <$> remove_child p q

    appendBuffer :: ParseConfig -> n Mutable -> S.ByteString -> IO Bool
    appendBuffer (ParseConfig (ParseFlags flags) enc) node str = fmap toBool $
        withNode node $ \n -> S.unsafeUseAsCStringLen str $ \(c,l) ->
        append_buffer n c (fromIntegral l) flags enc >>= parse_is_success

    selectSingleNode :: n m -> XPath NodeSet -> IO XPathNode
    selectSingleNode nd (XPath xp) =
        withNode nd $ \n -> withForeignPtr xp $ \x -> do
            select_single_node n x >>= peekXNode

    selectNodes :: n m -> XPath NodeSet -> IO NodeSet
    selectNodes nd (XPath xp) = withNode nd $ \n -> withForeignPtr xp $ \x -> do
        p <- select_nodes n x
        l <- fromIntegral <$> xpath_node_set_size p
        NodeSet l <$> newForeignPtr finalizerXpathNodeSet p

-- foreign import ccall unsafe print :: Ptr n -> FunPtr Writer -> CString -> CUInt -> Encoding -> CUInt -> IO ()

-- xpath_node
foreign import ccall unsafe delete_xpath_node :: Ptr XNode -> IO ()
foreign import ccall unsafe xpath_node_node :: Ptr XNode -> IO (Ptr Node)
foreign import ccall unsafe xpath_node_attribute :: Ptr XNode -> IO (Ptr Attr)

foreign import ccall unsafe "&delete_xpath_node_set" finalizerXpathNodeSet :: FinalizerPtr NodeSet
foreign import ccall unsafe xpath_node_set_size :: Ptr NodeSet -> IO CSize 

peekXNode :: Ptr XNode -> IO XPathNode
peekXNode p = do
    n <- xpath_node_node p
    if n == nullPtr
        then bracket (xpath_node_attribute p) delete_attr $ \a -> do
            an <- attrName a
            av <- attrValue a
            return $ Right (an, av)
        else Left . Node <$> newForeignPtr finalizerNode n


mapSiblingM :: NodeLike n => (Node_ m -> IO a) -> n m -> IO [a]
mapSiblingM func node = do
    ref <- newIORef id
    mapSiblingM_ (\n -> func n >>= \a -> modifyIORef ref (\f -> f . (a:))) node
    readIORef ref >>= \a -> return (a [])

mapAttrsM :: NodeLike n => (S.ByteString -> S.ByteString -> IO a) -> n m -> IO [a]
mapAttrsM func n = do
    ref <- newIORef id
    mapAttrsM_ (\k v -> func k v >>= \a -> modifyIORef ref (\f -> f . (a:))) n
    readIORef ref >>= \a -> return (a [])

instance NodeLike Document_ where withNode (Document f) = withForeignPtr f
instance NodeLike Node_     where withNode (Node     f) = withForeignPtr f
