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
import qualified Data.ByteString.Lazy as L

import Text.XML.Pugi.Foreign.Document
import Text.XML.Pugi.Foreign.Const
import Text.XML.Pugi.Foreign.Types
import Text.XML.Pugi.Foreign.Attr

-- node
foreign import ccall "&delete_node" finalizerNode
    :: FinalizerPtr (Node_ k m)

foreign import ccall document_element :: Ptr (Document_ k m) -> IO (Ptr (Node_ k m))

foreign import ccall node_equal :: Ptr a -> Ptr b -> IO CInt

foreign import ccall node_hash_value :: Ptr n -> IO CSize

foreign import ccall node_type :: Ptr n -> IO NodeType

foreign import ccall node_name  :: Ptr n -> IO CString
foreign import ccall node_value :: Ptr n -> IO CString

foreign import ccall node_parent           :: Ptr n -> IO (Ptr (Node_ Unknown a))
foreign import ccall node_first_child      :: Ptr n -> IO (Ptr (Node_ Unknown a))
foreign import ccall node_last_child       :: Ptr n -> IO (Ptr (Node_ Unknown a))
foreign import ccall node_next_sibling     :: Ptr n -> IO (Ptr (Node_ Unknown a))
foreign import ccall node_previous_sibling :: Ptr n -> IO (Ptr (Node_ Unknown a))

foreign import ccall node_child :: Ptr n -> CString -> IO (Ptr (Node_ Unknown a))
foreign import ccall node_attribute :: Ptr n -> CString -> IO (Ptr Attr)
foreign import ccall node_next_sibling_by_name :: Ptr n -> CString -> IO (Ptr (Node_ Unknown a))
foreign import ccall node_previous_sibling_by_name :: Ptr n -> CString -> IO (Ptr (Node_ Unknown a))
foreign import ccall node_find_child_by_name_and_attribute :: Ptr n -> CString -> CString -> CString -> IO (Ptr (Node_ Unknown a))
foreign import ccall node_find_child_by_attribute :: Ptr n -> CString -> CString -> IO (Ptr (Node_ Unknown a))

foreign import ccall node_child_value :: Ptr n -> IO CString
foreign import ccall node_child_value_by_name :: Ptr n -> CString -> IO CString
foreign import ccall node_text :: Ptr n -> IO CString

type NodeMapper k m = Ptr (Node_ k m) -> IO ()
foreign import ccall "wrapper" wrap_node_mapper :: NodeMapper k m -> IO (FunPtr (NodeMapper k m))
foreign import ccall node_map_sibling :: Ptr n -> FunPtr (NodeMapper k m) -> IO ()

type AttrPred = Ptr Attr -> IO CInt
foreign import ccall "wrapper" wrap_attr_pred :: AttrPred -> IO (FunPtr AttrPred)

type NodePred = Ptr Node -> IO CInt
foreign import ccall "wrapper" wrap_node_pred :: NodePred -> IO (FunPtr NodePred)

foreign import ccall find_attribute :: Ptr n -> FunPtr AttrPred -> IO (Ptr Attr)
foreign import ccall find_child     :: Ptr n -> FunPtr NodePred -> IO (Ptr (Node_ k m))
foreign import ccall find_node      :: Ptr n -> FunPtr NodePred -> IO (Ptr (Node_ k m))

type BeginEnd m = Ptr (Node_ Unknown m) -> IO CInt
type ForEach  m = CInt -> Ptr (Node_ Unknown m) -> IO CInt
foreign import ccall "wrapper" wrap_begin_end :: BeginEnd m -> IO (FunPtr (BeginEnd m))
foreign import ccall "wrapper" wrap_for_each  :: ForEach  m -> IO (FunPtr (ForEach  m))
foreign import ccall node_traverse :: Ptr n -> FunPtr (BeginEnd m) -> FunPtr (ForEach m) -> FunPtr (BeginEnd m) -> IO CInt

type AttrMapper = Ptr Attr -> IO ()
foreign import ccall "wrapper" wrap_attr_mapper :: AttrMapper -> IO (FunPtr AttrMapper)
foreign import ccall node_map_attributes :: Ptr n -> FunPtr AttrMapper -> IO ()

foreign import ccall node_path :: Ptr n -> CChar -> IO CString -- must be free

foreign import ccall node_first_element_by_path :: Ptr n -> CString -> CChar -> IO (Ptr (Node_ Unknown a))

foreign import ccall node_root :: Ptr n -> IO (Ptr (Node_ Unknown a))

foreign import ccall set_name  :: Ptr n -> CString -> IO CInt
foreign import ccall set_value :: Ptr n -> CString -> IO CInt

foreign import ccall append_attribute  :: Ptr n -> CString -> CString -> IO CInt
foreign import ccall prepend_attribute :: Ptr n -> CString -> CString -> IO CInt

foreign import ccall append_child  :: Ptr n -> NodeType -> IO (Ptr (Node_ k a))
foreign import ccall prepend_child :: Ptr n -> NodeType -> IO (Ptr (Node_ k a))

foreign import ccall append_copy  :: Ptr n -> Ptr (Node_ k a) -> IO (Ptr (Node_ k b))
foreign import ccall prepend_copy :: Ptr n -> Ptr (Node_ k a) -> IO (Ptr (Node_ k b))

foreign import ccall remove_attribute :: Ptr n -> CString -> IO CInt
foreign import ccall remove_child     :: Ptr n -> Ptr (Node_ k a) -> IO CInt

foreign import ccall append_buffer :: Ptr n -> Ptr c -> CSize -> CUInt -> Encoding -> IO ParseResult

foreign import ccall node_print :: Ptr n -> FunPtr Writer -> CString -> FormatFlags -> Encoding -> CUInt -> IO ()

foreign import ccall select_single_node :: Ptr n -> Ptr (XPath (NodeSet m)) -> IO (Ptr XNode)
foreign import ccall select_nodes :: Ptr n -> Ptr (XPath (NodeSet m)) -> IO (Ptr (NodeSet m))

nodeCommon :: NodeLike n => n k m -> (Ptr (n k m) -> IO (Ptr (Node_ l m))) -> IO (Maybe (Node_ l m))
nodeCommon n f = withNode n $ \p -> do
    q <- f p
    if q == nullPtr
    then return Nothing
    else Just . Node <$> newForeignPtr finalizerNode q

with_attr_pred :: (S.ByteString -> S.ByteString -> Bool) -> (FunPtr AttrPred -> IO a) -> IO a
with_attr_pred fn m = do
    let func p = do
            n <- S.packCString =<< attr_name  p
            v <- S.packCString =<< attr_value p
            return . fromBool $ fn n v
    bracket (wrap_attr_pred func) freeHaskellFunPtr m

with_node_pred :: (Node -> Bool) -> (FunPtr NodePred -> IO a) -> IO a
with_node_pred fn m =
    let func p = fromBool . fn . Node <$> newForeignPtr_ p
    in bracket (wrap_node_pred func) freeHaskellFunPtr m

class NodeLike (n :: NodeKind -> MutableFlag -> *) where
    withNode :: n k m -> (Ptr (n k m) -> IO a) -> IO a

    asNode :: n k m -> IO (Node_ k m)

    nodeEqual :: n k m -> n l o -> IO Bool
    nodeEqual a b = withNode a $ \p -> withNode b $ \q -> toBool <$> node_equal p q

    hashValue :: n k m -> IO CSize
    hashValue n = withNode n node_hash_value
    nodeType  :: n k m -> IO NodeType
    nodeType n = withNode n node_type

    getName  :: n k m -> IO S.ByteString
    getName n = withNode n $ node_name >=> S.packCString
    getValue :: n k m -> IO S.ByteString
    getValue n = withNode n $ node_value >=> S.packCString

    parent :: n k m -> IO (Maybe (Node_ Unknown m))
    parent n = nodeCommon n node_parent
    firstChild :: n k m -> IO (Maybe (Node_ Unknown m))
    firstChild n = nodeCommon n node_first_child
    lastChild :: n k m -> IO (Maybe (Node_ Unknown m))
    lastChild n = nodeCommon n node_last_child
    nextSibling :: n k m -> IO (Maybe (Node_ Unknown m))
    nextSibling n = nodeCommon n node_next_sibling
    prevSibling :: n k m -> IO (Maybe (Node_ Unknown m))
    prevSibling n = nodeCommon n node_previous_sibling

    child :: S.ByteString -> n k m -> IO (Maybe (Node_ Unknown m))
    child nm n = nodeCommon n $ \p -> S.useAsCString nm $ node_child p

    attribute :: S.ByteString -> n k m -> IO (Maybe S.ByteString)
    attribute nm n = withNode n $ \p -> S.useAsCString nm $ \a ->
        bracket (node_attribute p a) delete_attr $ \attr ->
        if attr == nullPtr
        then return Nothing
        else fmap Just $ attr_value attr >>= S.packCString

    nextSiblingByName :: S.ByteString -> n k m -> IO (Maybe (Node_ Unknown m))
    nextSiblingByName nm n = nodeCommon n $ \p -> S.useAsCString nm $ node_next_sibling_by_name p
    prevSiblingByName :: S.ByteString -> n k m -> IO (Maybe (Node_ Unknown m))
    prevSiblingByName nm n = nodeCommon n $ \p -> S.useAsCString nm $ node_previous_sibling_by_name p

    findChildByNameAndAttr :: S.ByteString -- ^ node name
                           -> S.ByteString -- ^ attribute name
                           -> S.ByteString -- ^ attribute value
                           -> n k m -> IO (Maybe (Node_ Unknown m))
    findChildByNameAndAttr nn an av n = nodeCommon n $ \p ->
        S.useAsCString nn $ \nn' -> S.useAsCString an $ \an' -> S.useAsCString av $ \av' ->
        node_find_child_by_name_and_attribute p nn' an' av'

    findChildByAttr :: S.ByteString -- ^ attribute name
                    -> S.ByteString -- ^ attribute value
                    -> n k m -> IO (Maybe (Node_ Unknown m))
    findChildByAttr an av n = nodeCommon n $ \p ->
        S.useAsCString an $ \an' -> S.useAsCString av $ \av' ->
        node_find_child_by_attribute p an' av'
    
    childValue :: n k m -> IO S.ByteString
    childValue n = withNode n $ node_child_value >=> S.packCString

    childValueByName :: S.ByteString -> n k m -> IO S.ByteString
    childValueByName nm n = withNode n $ \p -> S.useAsCString nm $ \s ->
        node_child_value_by_name p s >>= S.packCString

    text :: n k m -> IO S.ByteString
    text n = withNode n $ node_text >=> S.packCString

    findAttribute :: (S.ByteString -> S.ByteString -> Bool) -> n k m -> IO (Maybe Attribute)
    findAttribute f nd = withNode nd $ \n -> with_attr_pred f $ \a ->
        bracket (find_attribute n a) delete_attr $ \attr ->
            if attr == nullPtr
            then return Nothing
            else fmap Just $ (,) <$> (S.packCString =<< attr_name attr) <*> (S.packCString =<< attr_value attr)

    findChild :: (Node -> Bool) -> n k m -> IO (Maybe (Node_ Unknown m))
    findChild f nd = nodeCommon nd $ \n -> with_node_pred f $ find_child n

    findNode :: (Node -> Bool) -> n k m -> IO (Maybe (Node_ Unknown m))
    findNode f nd = nodeCommon nd $ \n -> with_node_pred f $ find_node n

    mapSiblingM_ :: (Node_ Unknown m -> IO ()) -> n k m -> IO ()
    mapSiblingM_ func n = withNode n $ \p -> do
        let f e = func . Node =<< newForeignPtr_ e
        bracket (wrap_node_mapper f) freeHaskellFunPtr $ node_map_sibling p

    mapAttrsM_ :: (S.ByteString -> S.ByteString -> IO ()) -> n k m -> IO ()
    mapAttrsM_ func n = withNode n $ \p -> do
        let f a = do
                nm  <- attrName a
                val <- attrValue a
                func nm val
        bracket (wrap_attr_mapper f) freeHaskellFunPtr $ node_map_attributes p

    path :: Char -> n k m -> IO S.ByteString
    path del node = withNode node $ \p ->
        bracket (node_path p (fromIntegral $ c2w del)) free $ S.packCString

    firstElementByPath :: Char -> S.ByteString -> n k m -> IO (Maybe (Node_ Unknown m))
    firstElementByPath del path_ node = nodeCommon node $ \p ->
        S.useAsCString path_ $ \d -> node_first_element_by_path p d (fromIntegral $ c2w del)

    root :: n k m -> IO (Maybe (Node_ Unknown m))
    root node = nodeCommon node $ node_root

    setName :: S.ByteString -> n k Mutable -> IO Bool
    setName n node = withNode node $ \p -> S.useAsCString n $ \q ->
        toBool <$> set_name p q
    setValue :: S.ByteString -> n k Mutable -> IO Bool
    setValue n node = withNode node $ \p -> S.useAsCString n $ \q ->
        toBool <$> set_value p q

    appendAttr :: S.ByteString -> S.ByteString -> n k Mutable -> IO Bool
    appendAttr k v n = withNode n $ \np -> S.useAsCString k $ \kp -> S.useAsCString v $ \vp ->
        toBool <$> append_attribute np kp vp
    prependAttr :: S.ByteString -> S.ByteString -> n k Mutable -> IO Bool
    prependAttr k v n = withNode n $ \np -> S.useAsCString k $ \kp -> S.useAsCString v $ \vp ->
        toBool <$> prepend_attribute np kp vp

    setAttr :: S.ByteString -> S.ByteString -> n k Mutable -> IO Bool
    setAttr k v n = withNode n $ \np -> S.useAsCString k $ \kp ->
        bracket (node_attribute np kp) delete_attr $ \attr ->
            if attr == nullPtr
                then return False
                else toBool <$> S.useAsCString v (attr_set_value attr)

    appendChild :: NodeType -> n k Mutable -> IO (Maybe (Node_ l Mutable))
    appendChild t n = nodeCommon n $ \p -> append_child p t
    prependChild :: NodeType -> n k Mutable -> IO (Maybe (Node_ l Mutable))
    prependChild t n = nodeCommon n $ \p -> prepend_child p t

    appendCopy :: Node_ k a -> n l Mutable -> IO (Maybe (Node_ k Mutable))
    appendCopy t n = nodeCommon n $ \p -> withNode t (append_copy p)
    prependCopy :: Node_ k a -> n l Mutable -> IO (Maybe (Node_ k Mutable))
    prependCopy t n = nodeCommon n $ \p -> withNode t (prepend_copy p)

    removeAttr :: S.ByteString -> n k Mutable -> IO Bool
    removeAttr a n = withNode n $ \p -> S.useAsCString a $ \c ->
        toBool <$> remove_attribute p c

    removeChild :: Node_ k a -> n l Mutable -> IO Bool
    removeChild c n = withNode n $ \p -> withNode c $ \q ->
        toBool <$> remove_child p q

    appendBuffer :: ParseConfig -> S.ByteString -> n k Mutable -> IO Bool
    appendBuffer (ParseConfig (ParseFlags flags) enc) str node = fmap toBool $
        withNode node $ \n -> S.unsafeUseAsCStringLen str $ \(c,l) ->
        bracket (append_buffer n c (fromIntegral l) flags enc) delete_parse_result
        parse_is_success

    selectSingleNode :: XPath (NodeSet m) -> n k m -> IO (XPathNode m)
    selectSingleNode (XPath xp) nd =
        withNode nd $ \n -> withForeignPtr xp $ \x -> do
            select_single_node n x >>= peekXNode

    selectNodes :: XPath (NodeSet m) -> n k m -> IO (NodeSet m)
    selectNodes (XPath xp) nd = withNode nd $ \n -> withForeignPtr xp $ \x -> do
        p <- select_nodes n x
        l <- fromIntegral <$> xpath_node_set_size p
        NodeSet l <$> newForeignPtr finalizerXpathNodeSet p

    prettyNode :: PrettyConfig -> Int -> n k m -> IO L.ByteString
    prettyNode (PrettyConfig indent flags enc) depth node =
        withNode node $ \n -> withCString indent $ \i -> do
            ref <- newIORef id
            let fun cs s = S.packCStringLen (cs, fromIntegral s) >>= \c -> modifyIORef ref (\a -> a . (c:))
            bracket (wrap_writer fun) freeHaskellFunPtr $ \fp ->
                node_print n fp i flags enc (fromIntegral depth)
            readIORef ref >>= \r -> return $ L.fromChunks (r [])

-- xpath_node
foreign import ccall delete_xpath_node :: Ptr XNode -> IO ()
foreign import ccall xpath_node_node :: Ptr XNode -> IO (Ptr (Node_ Unknown m))
foreign import ccall xpath_node_attribute :: Ptr XNode -> IO (Ptr Attr)

foreign import ccall "&delete_xpath_node_set" finalizerXpathNodeSet :: FinalizerPtr (NodeSet m)
foreign import ccall xpath_node_set_size :: Ptr (NodeSet m) -> IO CSize 

peekXNode :: Ptr XNode -> IO (XPathNode m)
peekXNode p = do
    n <- xpath_node_node p
    if n == nullPtr
        then bracket (xpath_node_attribute p) delete_attr $ \a -> do
            an <- attrName a
            av <- attrValue a
            return $ Right (an, av)
        else Left . Node <$> newForeignPtr finalizerNode n


mapSiblingM :: NodeLike n => (Node_ Unknown m -> IO a) -> n l m -> IO [a]
mapSiblingM func node = do
    ref <- newIORef id
    mapSiblingM_ (\n -> func n >>= \a -> modifyIORef ref (\f -> f . (a:))) node
    readIORef ref >>= \a -> return (a [])

mapAttrsM :: NodeLike n => (S.ByteString -> S.ByteString -> IO a) -> n k m -> IO [a]
mapAttrsM func n = do
    ref <- newIORef id
    mapAttrsM_ (\k v -> func k v >>= \a -> modifyIORef ref (\f -> f . (a:))) n
    readIORef ref >>= \a -> return (a [])

instance NodeLike Document_ where
    withNode (Document f) = withForeignPtr f
    asNode d = maybe (error "asNode return NULL") id <$> nodeCommon d document_element

instance NodeLike Node_     where
    withNode (Node     f) = withForeignPtr f
    asNode = return
