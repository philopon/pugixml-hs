{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Text.XML.Pugi.Mutable
    (
    -- ** setter
      Modify
    , create, modify
    , MutableNodeLike(..)
    , appendAttrs
    , setOrAppendAttr
    -- *** specified append/prepend child
    , appendElement, prependElement
    , appendDeclaration, prependDeclaration
    , appendPCData, prependPCData
    , appendCData, prependCData
    , appendComment, prependComment
    , appendDoctype, prependDoctype
    , appendPi, prependPi
    ) where

import Control.Applicative
import Control.Monad

import Foreign.C.Types

import           Text.XML.Pugi hiding (xpath)
import qualified Text.XML.Pugi.Foreign.Document as D
import qualified Text.XML.Pugi.Foreign.Node as N
import qualified Text.XML.Pugi.Foreign.XPath      as X

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import System.IO.Unsafe
import Unsafe.Coerce

newtype Modify a = Modify { runModify :: IO (Either String a) }
    deriving Functor

instance Applicative Modify where
    pure      = Modify . return . Right
    mf <*> ma = Modify $ runModify mf >>= \case
        Left  e -> return (Left e)
        Right f -> runModify ma >>= \case
            Left  e -> return (Left e)
            Right a -> return (Right (f a))

instance Monad Modify where
    return = pure
    ma >>= g = Modify $ runModify ma >>= \case
        Left  e -> return (Left e)
        Right a -> runModify $ g a
    fail = Modify . return . Left

instance Alternative Modify where
    empty = Modify . return $ Left "empty"
    ma <|> mb = Modify $ runModify ma >>= \case
        Left  _ -> runModify mb
        Right a -> return $ Right a

instance MonadPlus Modify where
    mzero = empty
    mplus = (<|>)

mLiftIO :: IO a -> Modify a
mLiftIO io = Modify $ Right <$> io

-- | create document from scratch.
create :: Monad m => (MutableDocument -> Modify ()) -> m Document
create m = either fail (return . D.freezeDocument) . unsafeDupablePerformIO . runModify $ do
    d <- mLiftIO D.createDocument
    m d
    return d

-- | modify document.
modify :: Monad m => Document -> (MutableDocument -> Modify ()) -> m Document
modify prt m = either fail (return . D.freezeDocument) . unsafePerformIO . runModify $ do
    d <- mLiftIO $ D.copyDocument prt
    m d
    return d

appendElement :: (HasChildren k, MutableNodeLike n)
              => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'Element)
appendElement n e = appendChild nodeTypeElement e >>= \r -> setName n r >> return r

prependElement :: (HasChildren k, MutableNodeLike n)
               => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'Element)
prependElement n e = prependChild nodeTypeElement e >>= \r -> setName n r >> return r

appendDeclaration :: (HasChildren k, MutableNodeLike n)
                  => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'Declaration)
appendDeclaration n e = appendChild nodeTypeDeclaration e >>= \r -> setName n r >> return r

prependDeclaration :: (HasChildren k, MutableNodeLike n)
                   => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'Declaration)
prependDeclaration n e = prependChild nodeTypeDeclaration e >>= \r -> setName n r >> return r

appendPCData :: (HasChildren k, MutableNodeLike n)
             => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'PCData)
appendPCData n e = appendChild nodeTypePCData e >>= \r -> setValue n r >> return r

prependPCData :: (HasChildren k, MutableNodeLike n)
              => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'PCData)
prependPCData n e = prependChild nodeTypePCData e >>= \r -> setValue n r >> return r

appendCData :: (HasChildren k, MutableNodeLike n)
            => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'CData)
appendCData n e = appendChild nodeTypeCData e >>= \r -> setValue n r >> return r

prependCData :: (HasChildren k, MutableNodeLike n)
             => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'CData)
prependCData n e = prependChild nodeTypeCData e >>= \r -> setValue n r >> return r

appendComment :: (HasChildren k, MutableNodeLike n)
              => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'Comment)
appendComment n e = appendChild nodeTypeComment e >>= \r -> setValue n r >> return r

prependComment :: (HasChildren k, MutableNodeLike n)
               => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'Comment)
prependComment n e = prependChild nodeTypeComment e >>= \r -> setValue n r >> return r

appendDoctype :: (HasChildren k, MutableNodeLike n)
              => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'Doctype)
appendDoctype n e = appendChild nodeTypeDoctype e >>= \r -> setValue n r >> return r

prependDoctype :: (HasChildren k, MutableNodeLike n)
               => S.ByteString -> n k 'Mutable -> Modify (MutableNode 'Doctype)
prependDoctype n e = prependChild nodeTypeDoctype e >>= \r -> setValue n r >> return r

appendPi :: (HasChildren k, MutableNodeLike n)
         => S.ByteString -> S.ByteString -> n k 'Mutable -> Modify (MutableNode 'Pi)
appendPi n v e = appendChild nodeTypePi e >>= \r -> setName n r >> setValue v r >> return r

prependPi :: (HasChildren k, MutableNodeLike n)
          => S.ByteString -> S.ByteString -> n k 'Mutable -> Modify (MutableNode 'Pi)
prependPi n v e = prependChild nodeTypePi e >>= \r -> setName n r >> setValue v r >> return r

class MutableNodeLike (n :: NodeKind -> MutableFlag -> *) where
    asNode                 :: n k 'Mutable -> Modify (Node_ k 'Mutable)
    nodeEqual              :: n k 'Mutable -> n l o -> Modify Bool
    forgetNodeKind         :: n k 'Mutable -> n 'Unknown 'Mutable
    forgetNodeKind = unsafeCoerce
    {-# INLINE forgetNodeKind #-}
    prettyNode             :: D.PrettyConfig -> Int -> n k 'Mutable -> Modify L.ByteString
    hashValue              :: n k 'Mutable -> Modify CSize
    nodeType               :: n k 'Mutable -> Modify NodeType
    getName                :: HasName  k => n k 'Mutable -> Modify S.ByteString
    getValue               :: HasValue k => n k 'Mutable -> Modify S.ByteString
    parent                 :: n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    firstChild             :: HasChildren k => n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    lastChild              :: HasChildren k => n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    nextSibling            :: n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    prevSibling            :: n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    child                  :: HasChildren  k => S.ByteString -> n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    attribute              :: HasAttribute k => S.ByteString -> n k 'Mutable -> Modify (Maybe S.ByteString)
    nextSiblingByName      :: S.ByteString -> n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    prevSiblingByName      :: S.ByteString -> n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    findChildByNameAndAttr :: HasChildren k
                           => S.ByteString -- ^ node name
                           -> S.ByteString -- ^ attribute name
                           -> S.ByteString -- ^ attribute value
                           -> n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    findChildByAttr        :: HasChildren k
                           => S.ByteString -- ^ attribute name
                           -> S.ByteString -- ^ attribute value
                           -> n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    childValue             :: HasChildren k => n k 'Mutable -> Modify S.ByteString
    childValueByName       :: HasChildren k => S.ByteString -> n k 'Mutable -> Modify S.ByteString
    text                   :: n k 'Mutable -> Modify S.ByteString

    -- | find attribute by predicate. since v0.2.0.
    findAttribute          :: (S.ByteString -> S.ByteString -> Bool) -> n k 'Mutable -> Modify (Maybe Attribute)

    -- | find child by predicate. since v0.2.0.
    findChild              :: (Node -> Bool) -> n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))

    -- | find node by predicate. since v0.2.0.
    findNode               :: (Node -> Bool) -> n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    path                   :: Char -> n k 'Mutable -> Modify S.ByteString
    firstElementByPath     :: Char -> S.ByteString -> n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    root                   :: n k 'Mutable -> Modify (Maybe (MutableNode 'Unknown))
    evaluate               :: X.EvalXPath r => XPath r -> n k 'Mutable -> Modify (X.XPathResult r 'Mutable)
    selectSingleNode       :: XPath NodeSet -> n k 'Mutable -> Modify (XPathNode 'Mutable)
    selectNodes            :: XPath NodeSet -> n k 'Mutable -> Modify (NodeSet 'Mutable)

    setName        :: HasName      k => S.ByteString -> n k 'Mutable -> Modify ()
    setValue       :: HasValue     k => S.ByteString -> n k 'Mutable -> Modify ()
    appendAttr     :: HasAttribute k => S.ByteString -> S.ByteString -> n k 'Mutable -> Modify ()
    prependAttr    :: HasAttribute k => S.ByteString -> S.ByteString -> n k 'Mutable -> Modify ()
    setAttr        :: HasAttribute k => S.ByteString -> S.ByteString -> n k 'Mutable -> Modify ()
    -- | generic appendChild method. Recommend to use 'appendElement' etc...
    appendChild    :: HasChildren  k => NodeType -> n k 'Mutable -> Modify (MutableNode l)
    -- | generic prependChild method. Recommend to use 'prependElement' etc...
    prependChild   :: HasChildren  k => NodeType -> n k 'Mutable -> Modify (MutableNode l)
    appendCopy     :: HasChildren  k => Node_ k a -> n l 'Mutable -> Modify (MutableNode k)
    prependCopy    :: HasChildren  k => Node_ k a -> n l 'Mutable -> Modify (MutableNode k)
    removeAttr     :: HasAttribute k => S.ByteString -> n k 'Mutable -> Modify ()
    removeChild    :: HasChildren  k => Node_ k a -> n l 'Mutable -> Modify ()
    appendFlagment :: HasChildren  k => D.ParseConfig -> S.ByteString -> n k 'Mutable -> Modify ()

    mapSiblingM  :: (MutableNode 'Unknown -> Modify a) -> n k 'Mutable -> Modify [a]
    mapSiblingM_ :: (MutableNode 'Unknown -> Modify a) -> n k 'Mutable -> Modify ()

appendAttrs :: (MutableNodeLike n, HasAttribute k) => [Attribute] ->  n k 'Mutable -> Modify ()
appendAttrs as n = mapM_ (\(k,v) -> appendAttr k v n) as

instance MutableNodeLike Node_ where
    asNode              = Modify . fmap Right . N.asNode
    nodeEqual a         = Modify . fmap Right . N.nodeEqual a
    prettyNode cfg dph  = Modify . fmap Right . N.prettyNode cfg dph
    hashValue           = Modify . fmap Right . N.hashValue
    nodeType            = Modify . fmap Right . N.nodeType
    getName             = Modify . fmap Right . N.getName
    getValue            = Modify . fmap Right . N.getValue
    parent              = Modify . fmap Right . N.parent
    firstChild          = Modify . fmap Right . N.firstChild
    lastChild           = Modify . fmap Right . N.lastChild
    nextSibling         = Modify . fmap Right . N.nextSibling
    prevSibling         = Modify . fmap Right . N.prevSibling
    child n             = Modify . fmap Right . N.child n
    attribute n         = Modify . fmap Right . N.attribute n
    nextSiblingByName n = Modify . fmap Right . N.nextSiblingByName n
    prevSiblingByName n = Modify . fmap Right . N.prevSiblingByName n
    findChildByNameAndAttr nn an av =
        Modify . fmap Right . N.findChildByNameAndAttr nn an av
    findChildByAttr an av  = Modify . fmap Right . N.findChildByAttr an av
    childValue             = Modify . fmap Right . N.childValue
    childValueByName n     = Modify . fmap Right . N.childValueByName n
    text                   = Modify . fmap Right . N.text
    findAttribute f        = Modify . fmap Right . N.findAttribute f
    findChild f            = Modify . fmap Right . N.findChild f
    findNode f             = Modify . fmap Right . N.findNode f
    path c                 = Modify . fmap Right . N.path c
    firstElementByPath c p = Modify . fmap Right . N.firstElementByPath c p
    root                   = Modify . fmap Right . N.root
    evaluate x             = Modify . fmap Right . X.evaluateXPath x
    selectSingleNode x     = Modify . fmap Right . N.selectSingleNode x
    selectNodes x          = Modify . fmap Right . N.selectNodes x

    setName        = isetName
    setValue       = isetValue
    appendAttr     = iappendAttr
    prependAttr    = iprependAttr
    setAttr        = isetAttr
    appendChild    = iappendChild
    prependChild   = iprependChild
    appendCopy     = iappendCopy
    prependCopy    = iprependCopy
    removeAttr     = iremoveAttr
    removeChild    = iremoveChild
    appendFlagment = iappendFlagment

    mapSiblingM  f = Modify . fmap sequence  . N.mapSiblingM (runModify . f)
    mapSiblingM_ f = Modify . fmap sequence_ . N.mapSiblingM (runModify . f)

instance MutableNodeLike Document_ where
    asNode              = Modify . fmap Right . N.asNode
    nodeEqual a         = Modify . fmap Right . N.nodeEqual a
    prettyNode cfg dph  = Modify . fmap Right . N.prettyNode cfg dph
    hashValue           = Modify . fmap Right . N.hashValue
    nodeType            = Modify . fmap Right . N.nodeType
    getName             = Modify . fmap Right . N.getName
    getValue            = Modify . fmap Right . N.getValue
    parent              = Modify . fmap Right . N.parent
    firstChild          = Modify . fmap Right . N.firstChild
    lastChild           = Modify . fmap Right . N.lastChild
    nextSibling         = Modify . fmap Right . N.nextSibling
    prevSibling         = Modify . fmap Right . N.prevSibling
    child n             = Modify . fmap Right . N.child n
    attribute n         = Modify . fmap Right . N.attribute n
    nextSiblingByName n = Modify . fmap Right . N.nextSiblingByName n
    prevSiblingByName n = Modify . fmap Right . N.prevSiblingByName n
    findChildByNameAndAttr nn an av =
        Modify . fmap Right . N.findChildByNameAndAttr nn an av
    findChildByAttr an av  = Modify . fmap Right . N.findChildByAttr an av
    childValue             = Modify . fmap Right . N.childValue
    childValueByName n     = Modify . fmap Right . N.childValueByName n
    text                   = Modify . fmap Right . N.text
    findAttribute f        = Modify . fmap Right . N.findAttribute f
    findChild f            = Modify . fmap Right . N.findChild f
    findNode f             = Modify . fmap Right . N.findNode f
    path c                 = Modify . fmap Right . N.path c
    firstElementByPath c p = Modify . fmap Right . N.firstElementByPath c p
    root                   = Modify . fmap Right . N.root
    evaluate x             = Modify . fmap Right . X.evaluateXPath x
    selectSingleNode x     = Modify . fmap Right . N.selectSingleNode x
    selectNodes x          = Modify . fmap Right . N.selectNodes x

    setName        = isetName
    setValue       = isetValue
    appendAttr     = iappendAttr
    prependAttr    = iprependAttr
    setAttr        = isetAttr
    appendChild    = iappendChild
    prependChild   = iprependChild
    appendCopy     = iappendCopy
    prependCopy    = iprependCopy
    removeAttr     = iremoveAttr
    removeChild    = iremoveChild
    appendFlagment = iappendFlagment

    mapSiblingM  f = Modify . fmap sequence  . N.mapSiblingM (runModify . f)
    mapSiblingM_ f = Modify . fmap sequence_ . N.mapSiblingM (runModify . f)

setOrAppendAttr :: (HasAttribute k, MutableNodeLike n)
                => S.ByteString -> S.ByteString -> n k 'Mutable -> Modify ()
setOrAppendAttr k v n = setAttr k v n <|> appendAttr k v n

isetName :: N.NodeLike n => S.ByteString -> n k 'Mutable -> Modify ()
isetName n nd = mLiftIO (N.setName n nd) >>= 
    flip unless (fail $ "setName: " ++ show n)

isetValue :: N.NodeLike n => S.ByteString -> n k 'Mutable -> Modify ()
isetValue n nd = mLiftIO (N.setValue n nd) >>=
    flip unless (fail $ "setValue: " ++ show n)

iappendAttr :: N.NodeLike n => S.ByteString -> S.ByteString
            -> n k 'Mutable -> Modify ()
iappendAttr k v n = mLiftIO (N.appendAttr k v n) >>=
    flip unless (fail $ "appendAttr: " ++ show k ++ " = " ++ show v)

iprependAttr :: N.NodeLike n => S.ByteString -> S.ByteString
             -> n k 'Mutable -> Modify ()
iprependAttr k v n = mLiftIO (N.prependAttr k v n) >>=
    flip unless (fail $ "appendAttr: " ++ show k ++ " = " ++ show v)

isetAttr :: N.NodeLike n => S.ByteString -> S.ByteString
         -> n k 'Mutable -> Modify ()
isetAttr k v n = mLiftIO (N.setAttr k v n) >>=
    flip unless (fail $ "setAttr: " ++ show k ++ " = " ++ show v)

iappendChild :: N.NodeLike n => NodeType -> n l 'Mutable -> Modify (MutableNode k)
iappendChild t n = mLiftIO (N.appendChild t n) >>=
    maybe (fail $ "appendChild: " ++ show t) return

iprependChild :: N.NodeLike n => NodeType -> n l 'Mutable -> Modify (MutableNode k)
iprependChild t n = mLiftIO (N.prependChild t n) >>=
    maybe (fail $ "prependChild: " ++ show t) return

iappendCopy :: N.NodeLike n => Node_ k a -> n l 'Mutable -> Modify (MutableNode k)
iappendCopy t n = mLiftIO (N.appendCopy t n) >>=
    maybe (fail "appendCopy") return

iprependCopy :: N.NodeLike n => Node_ k a -> n l 'Mutable -> Modify (MutableNode k)
iprependCopy t n = mLiftIO (N.prependCopy t n) >>=
    maybe (fail "prependCopy") return

iremoveAttr :: N.NodeLike n => S.ByteString -> n k 'Mutable -> Modify ()
iremoveAttr n nd = mLiftIO (N.removeAttr n nd) >>=
    flip unless (fail $ "removeAttr: " ++ show n)

iremoveChild :: N.NodeLike n => Node_ l a -> n k 'Mutable -> Modify ()
iremoveChild n nd = mLiftIO (N.removeChild n nd) >>=
    flip unless (fail "removeChild")

iappendFlagment :: N.NodeLike n => D.ParseConfig -> S.ByteString -> n k 'Mutable -> Modify ()
iappendFlagment cfg str n = mLiftIO (N.appendBuffer cfg str n) >>=
    flip unless (fail $ "appendFlagment: " ++ show str)
