{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

-- | parse xml
--
-- @
-- \> docA <- parse def \"\<a \/\>\"
-- \> docA
-- Right Document \<a \/\>
-- 
-- \> parseFile def \"test.xml\"
-- Document \<test \>
-- @
--
-- render xml
--
-- @
-- \> Data.ByteString.Lazy.Char.putStrLn $ either undefined (pretty def) docA
-- \<?xml version=\"1.0\"?\>
-- \<a \/\>
--
-- \> prettyFile def docA
-- @
--
-- create xml
--
-- @
-- testHtml :: IO 'Document'
-- testHtml = 'create' $ \doc -\> do
--     decl <- 'appendDeclaration' \"xml\" doc
--     'appendAttrs' [(\"version\", \"1.0\"), (\"lang\", \"ja\")] decl
--
--     'appendDoctype' \"html\" doc
--
--     html <- 'appendElement' \"html\" doc
--     body <- appendElement \"body\" html
--     div_ <- appendElement \"div\"  body
--     a    <- appendElement \"a\"    div_
--     'appendAttr' \"href\" \"http:\/\/example.com\" a
--     txt  <- 'appendPCData' \"example.com\" a
--     return ()
-- @
--
-- @
-- -- testHtml for copy&paste to ghci.
-- \> doc \<- create $ \\doc -\> appendDeclaration \"xml\" doc >>= \\decl -\> appendAttrs [(\"version\", \"1.0\"), (\"lang\", \"ja\")] decl >> appendDoctype \"html\" doc >> appendElement \"html\" doc >>= \\html -\> appendElement \"body\" html >>= \\body -\> appendElement \"div\"  body >>= \\div_ -\> appendElement \"a\"    div_ >>= \\a -\> appendAttr \"href\" \"http:\/\/example.com\" a >> appendPCData \"example.com\" a >> return ()
--
-- \> doc
-- Document \<?xml version=\"1.0\" lang=\"ja\"?\>\<!DOCTYPE html\>\<html\>\<body\>\<div\>\<a href=\"http:\/\/example.com\"\>example.com\<\/a\>\<\/div\>\<\/body\>\<\/html\>
-- @
--
-- access xml tree
--
-- @
-- \> let Just x = 'child' \"xml\" doc
-- \> x
-- Node \<?xml version=\"1.0\" lang=\"ja\"?\>
--
-- \> 'nextSibling' x
-- Just Node \<!DOCTYPE html\>
--
-- \> Just html = 'nextSiblingByName' "html" x
--
-- \> html
-- Node \<html\>\<body\>\<div\>\<a href=\"http:\/\/example.com\"\>example.com\<\/a\>\<\/div\>\<\/body\>\<\/html\>
--
-- \> 'evaluate' [xpath|string(\/\/a\/@href)|] html
-- \"http:\/\/example.com\"
--
-- \> let ns = evaluate [xpath|\/\/a\/@href|] html
--
-- \> 'nodeSetSize' ns
-- 1
-- \> 'nodeSetIndex' ns 0
-- Right (\"href\",\"http:\/\/example.com\")
-- @
--
-- modify xml
--
-- @
-- modify doc $ \\d -\> 'selectSingleNode' [xpath|\/\/a|] d >>= \\(Left a) -\> setOrAppendAttr \"href\" \"#\" a
-- Document \<?xml version=\"1.0\" lang=\"ja\"?\>\<!DOCTYPE html\>\<html\>\<body\>\<div\>\<a href=\"#\"\>example.com\<\/a\>\<\/div\>\<\/body\>\<\/html\>
-- @
-- 

module Text.XML.Pugi
    ( -- * Document
      Document_, Document, MutableDocument
      -- ** parse
    , D.ParseConfig(..), D.ParseException(..)
    , parse, D.parseFile
      -- ** render
    , D.PrettyConfig(..)
    , D.prettyFile, pretty

      -- * Node
    , Node_, Node, MutableNode
    , NodeKind(..)
    , HasName, HasValue, HasAttribute, HasChildren

    -- ** getter
    , M, NodeLike(..)

    -- ** setter
    , Modify
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

    -- * XPath
    , XPath
    , X.EvalXPath
    , X.xpath
    -- ** NodeSet
    , NodeSet, XPathNode, Attribute
    , X.nodeSetSize
    , nodeSetIndex
    , X.nodeSetMapM, X.nodeSetMapM_, nodeSetMap, nodeSetToList

    -- * reexport
    , module Text.XML.Pugi.Const
    , def
    ) where

import Control.Applicative
import Control.Monad

import Foreign.C.Types

import           Text.XML.Pugi.Const
import           Text.XML.Pugi.Foreign.Types
import qualified Text.XML.Pugi.Foreign.Document as D
import qualified Text.XML.Pugi.Foreign.Node as N
import qualified Text.XML.Pugi.Foreign.XPath      as X
import qualified Text.XML.Pugi.Foreign.XPath.Node as X

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Default.Class

import System.IO.Unsafe
import Unsafe.Coerce

parse :: D.ParseConfig -> S.ByteString
      -> Either D.ParseException Document
parse cfg str = unsafePerformIO $ D.parse cfg str
{-# NOINLINE parse #-}

pretty :: D.PrettyConfig -> Document -> L.ByteString
pretty cfg doc = unsafeDupablePerformIO $ D.pretty cfg doc

instance Show (Node_ k Immutable) where
    show = ("Node " ++) . L8.unpack . prettyNode def {D.prettyFlags = formatRaw} 0

instance Show (Document_ k Immutable) where
    show = ("Document " ++) . L8.unpack . prettyNode def {D.prettyFlags = formatRaw} 0

instance Eq (Node_ k Immutable) where
    (==) = nodeEqual

instance Eq (Document_ k Immutable) where
    (==) = nodeEqual

-- |
-- @
-- M Immutable a = a
-- M Mutable   a = 'Modify' a
-- @
--
type family M (m :: MutableFlag) a
type instance M Immutable a = a
type instance M Mutable   a = Modify a

-- |
-- @
-- instance NodeLike 'Document_' Immutable
-- instance NodeLike 'Node_'     Immutable
-- instance NodeLike 'Document_' Mutable
-- instance NodeLike 'Node_'     Mutable
-- @
--
class NodeLike n m where
    asNode                 :: n k m -> M m (Node_ k m)
    nodeEqual              :: n k m -> n l o -> M m Bool
    forgetNodeKind         :: n k m -> n Unknown m
    forgetNodeKind = unsafeCoerce
    {-# INLINE forgetNodeKind #-}
    prettyNode             :: D.PrettyConfig -> Int -> n k m -> M m L.ByteString
    hashValue              :: n k m -> M m CSize
    nodeType               :: n k m -> M m NodeType
    getName                :: HasName  k => n k m -> M m S.ByteString
    getValue               :: HasValue k => n k m -> M m S.ByteString
    parent                 :: n k m -> M m (Maybe (Node_ Unknown m))
    firstChild             :: HasChildren k => n k m -> M m (Maybe (Node_ Unknown m))
    lastChild              :: HasChildren k => n k m -> M m (Maybe (Node_ Unknown m))
    nextSibling            :: n k m -> M m (Maybe (Node_ Unknown m))
    prevSibling            :: n k m -> M m (Maybe (Node_ Unknown m))
    child                  :: HasChildren  k => S.ByteString -> n k m -> M m (Maybe (Node_ Unknown m))
    attribute              :: HasAttribute k => S.ByteString -> n k m -> M m (Maybe S.ByteString)
    nextSiblingByName      :: S.ByteString -> n k m -> M m (Maybe (Node_ Unknown m))
    prevSiblingByName      :: S.ByteString -> n k m -> M m (Maybe (Node_ Unknown m))
    findChildByNameAndAttr :: HasChildren k
                           => S.ByteString -- ^ node name
                           -> S.ByteString -- ^ attribute name
                           -> S.ByteString -- ^ attribute value
                           -> n k m -> M m (Maybe (Node_ Unknown m))
    findChildByAttr        :: HasChildren k
                           => S.ByteString -- ^ attribute name
                           -> S.ByteString -- ^ attribute value
                           -> n k m -> M m (Maybe (Node_ Unknown m))
    childValue             :: HasChildren k => n k m -> M m S.ByteString
    childValueByName       :: HasChildren k => S.ByteString -> n k m -> M m S.ByteString
    text                   :: n k m -> M m S.ByteString

    -- | find attribute by predicate. since v0.2.0.
    findAttribute          :: (S.ByteString -> S.ByteString -> Bool) -> n k m -> M m (Maybe Attribute)

    -- | find child by predicate. since v0.2.0.
    findChild              :: (Node -> Bool) -> n k m -> M m (Maybe (Node_ Unknown m))

    -- | find node by predicate. since v0.2.0.
    findNode               :: (Node -> Bool) -> n k m -> M m (Maybe (Node_ Unknown m))
    mapSibling             :: (Node_ Unknown m -> a) -> n k m -> M m [a]
    mapAttrs               :: HasAttribute k => (S.ByteString -> S.ByteString -> a) -> n k m -> M m [a]
    path                   :: Char -> n k m -> M m S.ByteString
    firstElementByPath     :: Char -> S.ByteString -> n k m -> M m (Maybe (Node_ Unknown m))
    root                   :: n k m -> M m (Maybe (Node_ Unknown m))
    evaluate               :: X.EvalXPath r => XPath r -> n k m -> M m r
    selectSingleNode       :: XPath (NodeSet m) -> n k m -> M m (XPathNode m)
    selectNodes            :: XPath (NodeSet m) -> n k m -> M m (NodeSet m)

instance NodeLike Document_ Immutable where
    asNode              = unsafeDupablePerformIO . N.asNode
    nodeEqual a         = unsafeDupablePerformIO . N.nodeEqual a
    prettyNode cfg dph  = unsafeDupablePerformIO . N.prettyNode cfg dph
    hashValue           = unsafeDupablePerformIO . N.hashValue
    nodeType            = unsafeDupablePerformIO . N.nodeType
    getName             = unsafeDupablePerformIO . N.getName
    getValue            = unsafeDupablePerformIO . N.getValue
    parent              = unsafeDupablePerformIO . N.parent
    firstChild          = unsafeDupablePerformIO . N.firstChild
    lastChild           = unsafeDupablePerformIO . N.lastChild
    nextSibling         = unsafeDupablePerformIO . N.nextSibling
    prevSibling         = unsafeDupablePerformIO . N.prevSibling
    child n             = unsafeDupablePerformIO . N.child n
    attribute n         = unsafeDupablePerformIO . N.attribute n
    nextSiblingByName n = unsafeDupablePerformIO . N.nextSiblingByName n
    prevSiblingByName n = unsafeDupablePerformIO . N.prevSiblingByName n
    findChildByNameAndAttr nn an av =
        unsafeDupablePerformIO . N.findChildByNameAndAttr nn an av
    findChildByAttr an av  = unsafeDupablePerformIO . N.findChildByAttr an av
    childValue             = unsafeDupablePerformIO . N.childValue
    childValueByName n     = unsafeDupablePerformIO . N.childValueByName n
    text                   = unsafeDupablePerformIO . N.text
    findAttribute f        = unsafeDupablePerformIO . N.findAttribute f
    findChild f            = unsafeDupablePerformIO . N.findChild f
    findNode f             = unsafeDupablePerformIO . N.findNode f
    mapSibling f           = unsafeDupablePerformIO . N.mapSiblingM (return . f)
    mapAttrs f             = unsafeDupablePerformIO . N.mapAttrsM (\k v -> return $ f k v)
    path c                 = unsafeDupablePerformIO . N.path c
    firstElementByPath c p = unsafeDupablePerformIO . N.firstElementByPath c p
    root                   = unsafeDupablePerformIO . N.root
    evaluate x             = unsafeDupablePerformIO . X.evaluateXPath x
    selectSingleNode x     = unsafeDupablePerformIO . N.selectSingleNode x
    selectNodes x          = unsafeDupablePerformIO . N.selectNodes x

instance NodeLike Node_ Immutable where
    asNode              = unsafeDupablePerformIO . N.asNode
    nodeEqual a         = unsafeDupablePerformIO . N.nodeEqual a
    prettyNode cfg dph  = unsafeDupablePerformIO . N.prettyNode cfg dph
    hashValue           = unsafeDupablePerformIO . N.hashValue
    nodeType            = unsafeDupablePerformIO . N.nodeType
    getName             = unsafeDupablePerformIO . N.getName
    getValue            = unsafeDupablePerformIO . N.getValue
    parent              = unsafeDupablePerformIO . N.parent
    firstChild          = unsafeDupablePerformIO . N.firstChild
    lastChild           = unsafeDupablePerformIO . N.lastChild
    nextSibling         = unsafeDupablePerformIO . N.nextSibling
    prevSibling         = unsafeDupablePerformIO . N.prevSibling
    child n             = unsafeDupablePerformIO . N.child n
    attribute n         = unsafeDupablePerformIO . N.attribute n
    nextSiblingByName n = unsafeDupablePerformIO . N.nextSiblingByName n
    prevSiblingByName n = unsafeDupablePerformIO . N.prevSiblingByName n
    findChildByNameAndAttr nn an av =
        unsafeDupablePerformIO . N.findChildByNameAndAttr nn an av
    findChildByAttr an av  = unsafeDupablePerformIO . N.findChildByAttr an av
    childValue             = unsafeDupablePerformIO . N.childValue
    childValueByName n     = unsafeDupablePerformIO . N.childValueByName n
    text                   = unsafeDupablePerformIO . N.text
    findAttribute f        = unsafeDupablePerformIO . N.findAttribute f
    findChild f            = unsafeDupablePerformIO . N.findChild f
    findNode f             = unsafeDupablePerformIO . N.findNode f
    mapSibling f           = unsafeDupablePerformIO . N.mapSiblingM (return . f)
    mapAttrs f             = unsafeDupablePerformIO . N.mapAttrsM (\k v -> return $ f k v)
    path c                 = unsafeDupablePerformIO . N.path c
    firstElementByPath c p = unsafeDupablePerformIO . N.firstElementByPath c p
    root                   = unsafeDupablePerformIO . N.root
    evaluate x             = unsafeDupablePerformIO . X.evaluateXPath x
    selectSingleNode x     = unsafeDupablePerformIO . N.selectSingleNode x
    selectNodes x          = unsafeDupablePerformIO . N.selectNodes x

instance NodeLike Document_ Mutable where
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
    mapSibling f           = Modify . fmap Right . N.mapSiblingM (return . f)
    mapAttrs f             = Modify . fmap Right . N.mapAttrsM (\k v -> return $ f k v)
    path c                 = Modify . fmap Right . N.path c
    firstElementByPath c p = Modify . fmap Right . N.firstElementByPath c p
    root                   = Modify . fmap Right . N.root
    evaluate x             = Modify . fmap Right . X.evaluateXPath x
    selectSingleNode x     = Modify . fmap Right . N.selectSingleNode x
    selectNodes x          = Modify . fmap Right . N.selectNodes x

instance NodeLike Node_ Mutable where
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
    mapSibling f           = Modify . fmap Right . N.mapSiblingM (return . f)
    mapAttrs f             = Modify . fmap Right . N.mapAttrsM (\k v -> return $ f k v)
    path c                 = Modify . fmap Right . N.path c
    firstElementByPath c p = Modify . fmap Right . N.firstElementByPath c p
    root                   = Modify . fmap Right . N.root
    evaluate x             = Modify . fmap Right . X.evaluateXPath x
    selectSingleNode x     = Modify . fmap Right . N.selectSingleNode x
    selectNodes x          = Modify . fmap Right . N.selectNodes x

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
              => S.ByteString -> n k Mutable -> Modify (MutableNode Element)
appendElement n e = appendChild nodeTypeElement e >>= \r -> setName n r >> return r

prependElement :: (HasChildren k, MutableNodeLike n)
               => S.ByteString -> n k Mutable -> Modify (MutableNode Element)
prependElement n e = prependChild nodeTypeElement e >>= \r -> setName n r >> return r

appendDeclaration :: (HasChildren k, MutableNodeLike n)
                  => S.ByteString -> n k Mutable -> Modify (MutableNode Declaration)
appendDeclaration n e = appendChild nodeTypeDeclaration e >>= \r -> setName n r >> return r

prependDeclaration :: (HasChildren k, MutableNodeLike n)
                   => S.ByteString -> n k Mutable -> Modify (MutableNode Declaration)
prependDeclaration n e = prependChild nodeTypeDeclaration e >>= \r -> setName n r >> return r

appendPCData :: (HasChildren k, MutableNodeLike n)
             => S.ByteString -> n k Mutable -> Modify (MutableNode PCData)
appendPCData n e = appendChild nodeTypePCData e >>= \r -> setValue n r >> return r

prependPCData :: (HasChildren k, MutableNodeLike n)
              => S.ByteString -> n k Mutable -> Modify (MutableNode PCData)
prependPCData n e = prependChild nodeTypePCData e >>= \r -> setValue n r >> return r

appendCData :: (HasChildren k, MutableNodeLike n)
            => S.ByteString -> n k Mutable -> Modify (MutableNode CData)
appendCData n e = appendChild nodeTypeCData e >>= \r -> setValue n r >> return r

prependCData :: (HasChildren k, MutableNodeLike n)
             => S.ByteString -> n k Mutable -> Modify (MutableNode CData)
prependCData n e = prependChild nodeTypeCData e >>= \r -> setValue n r >> return r

appendComment :: (HasChildren k, MutableNodeLike n)
              => S.ByteString -> n k Mutable -> Modify (MutableNode Comment)
appendComment n e = appendChild nodeTypeComment e >>= \r -> setValue n r >> return r

prependComment :: (HasChildren k, MutableNodeLike n)
               => S.ByteString -> n k Mutable -> Modify (MutableNode Comment)
prependComment n e = prependChild nodeTypeComment e >>= \r -> setValue n r >> return r

appendDoctype :: (HasChildren k, MutableNodeLike n)
              => S.ByteString -> n k Mutable -> Modify (MutableNode Doctype)
appendDoctype n e = appendChild nodeTypeDoctype e >>= \r -> setValue n r >> return r

prependDoctype :: (HasChildren k, MutableNodeLike n)
               => S.ByteString -> n k Mutable -> Modify (MutableNode Doctype)
prependDoctype n e = prependChild nodeTypeDoctype e >>= \r -> setValue n r >> return r

appendPi :: (HasChildren k, MutableNodeLike n)
         => S.ByteString -> S.ByteString -> n k Mutable -> Modify (MutableNode Pi)
appendPi n v e = appendChild nodeTypePi e >>= \r -> setName n r >> setValue v r >> return r

prependPi :: (HasChildren k, MutableNodeLike n)
          => S.ByteString -> S.ByteString -> n k Mutable -> Modify (MutableNode Pi)
prependPi n v e = prependChild nodeTypePi e >>= \r -> setName n r >> setValue v r >> return r

class HasName (k :: NodeKind)
instance HasName Element
instance HasName Declaration
instance HasName Pi
instance HasName Unknown

class HasValue (k :: NodeKind)
instance HasValue PCData
instance HasValue CData
instance HasValue Comment
instance HasValue Doctype
instance HasValue Pi
instance HasValue Unknown

class HasAttribute (k :: NodeKind)
instance HasAttribute Element
instance HasAttribute Declaration
instance HasAttribute Unknown

class HasChildren (k :: NodeKind)
instance HasChildren Element
instance HasChildren Unknown

class MutableNodeLike (n :: NodeKind -> MutableFlag -> *) where
    setName        :: HasName      k => S.ByteString -> n k Mutable -> Modify ()
    setValue       :: HasValue     k => S.ByteString -> n k Mutable -> Modify ()
    appendAttr     :: HasAttribute k => S.ByteString -> S.ByteString -> n k Mutable -> Modify ()
    prependAttr    :: HasAttribute k => S.ByteString -> S.ByteString -> n k Mutable -> Modify ()
    setAttr        :: HasAttribute k => S.ByteString -> S.ByteString -> n k Mutable -> Modify ()
    -- | generic appendChild method. Recommend to use 'appendElement' etc...
    appendChild    :: HasChildren  k => NodeType -> n k Mutable -> Modify (MutableNode l)
    -- | generic prependChild method. Recommend to use 'prependElement' etc...
    prependChild   :: HasChildren  k => NodeType -> n k Mutable -> Modify (MutableNode l)
    appendCopy     :: HasChildren  k => Node_ k a -> n l Mutable -> Modify (MutableNode k)
    prependCopy    :: HasChildren  k => Node_ k a -> n l Mutable -> Modify (MutableNode k)
    removeAttr     :: HasAttribute k => S.ByteString -> n k Mutable -> Modify ()
    removeChild    :: HasChildren  k => Node_ k a -> n l Mutable -> Modify ()
    appendFlagment :: HasChildren  k => D.ParseConfig -> S.ByteString -> n k Mutable -> Modify ()

appendAttrs :: (MutableNodeLike n, HasAttribute k) => [Attribute] ->  n k Mutable -> Modify ()
appendAttrs as n = mapM_ (\(k,v) -> appendAttr k v n) as

instance MutableNodeLike Node_ where
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

instance MutableNodeLike Document_ where
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

setOrAppendAttr :: (HasAttribute k, MutableNodeLike n)
                => S.ByteString -> S.ByteString -> n k Mutable -> Modify ()
setOrAppendAttr k v n = setAttr k v n <|> appendAttr k v n

isetName :: N.NodeLike n => S.ByteString -> n k Mutable -> Modify ()
isetName n nd = mLiftIO (N.setName n nd) >>= 
    flip unless (fail $ "setName: " ++ show n)

isetValue :: N.NodeLike n => S.ByteString -> n k Mutable -> Modify ()
isetValue n nd = mLiftIO (N.setValue n nd) >>=
    flip unless (fail $ "setValue: " ++ show n)

iappendAttr :: N.NodeLike n => S.ByteString -> S.ByteString
            -> n k Mutable -> Modify ()
iappendAttr k v n = mLiftIO (N.appendAttr k v n) >>=
    flip unless (fail $ "appendAttr: " ++ show k ++ " = " ++ show v)

iprependAttr :: N.NodeLike n => S.ByteString -> S.ByteString
             -> n k Mutable -> Modify ()
iprependAttr k v n = mLiftIO (N.prependAttr k v n) >>=
    flip unless (fail $ "appendAttr: " ++ show k ++ " = " ++ show v)

isetAttr :: N.NodeLike n => S.ByteString -> S.ByteString
         -> n k Mutable -> Modify ()
isetAttr k v n = mLiftIO (N.setAttr k v n) >>=
    flip unless (fail $ "setAttr: " ++ show k ++ " = " ++ show v)

iappendChild :: N.NodeLike n => NodeType -> n l Mutable -> Modify (MutableNode k)
iappendChild t n = mLiftIO (N.appendChild t n) >>=
    maybe (fail $ "appendChild: " ++ show t) return

iprependChild :: N.NodeLike n => NodeType -> n l Mutable -> Modify (MutableNode k)
iprependChild t n = mLiftIO (N.prependChild t n) >>=
    maybe (fail $ "prependChild: " ++ show t) return

iappendCopy :: N.NodeLike n => Node_ k a -> n l Mutable -> Modify (MutableNode k)
iappendCopy t n = mLiftIO (N.appendCopy t n) >>=
    maybe (fail "appendCopy") return

iprependCopy :: N.NodeLike n => Node_ k a -> n l Mutable -> Modify (MutableNode k)
iprependCopy t n = mLiftIO (N.prependCopy t n) >>=
    maybe (fail "prependCopy") return

iremoveAttr :: N.NodeLike n => S.ByteString -> n k Mutable -> Modify ()
iremoveAttr n nd = mLiftIO (N.removeAttr n nd) >>=
    flip unless (fail $ "removeAttr: " ++ show n)

iremoveChild :: N.NodeLike n => Node_ l a -> n k Mutable -> Modify ()
iremoveChild n nd = mLiftIO (N.removeChild n nd) >>=
    flip unless (fail "removeChild")

iappendFlagment :: N.NodeLike n => D.ParseConfig -> S.ByteString -> n k Mutable -> Modify ()
iappendFlagment cfg str n = mLiftIO (N.appendBuffer cfg str n) >>=
    flip unless (fail $ "appendFlagment: " ++ show str)

nodeSetIndex :: NodeSet m -> Int -> XPathNode m
nodeSetIndex n = unsafeDupablePerformIO . X.nodeSetIndex n

nodeSetMap :: (XPathNode m -> a) -> NodeSet m -> [a]
nodeSetMap f = unsafeDupablePerformIO . X.nodeSetMapM (return . f)

nodeSetToList :: NodeSet m -> [XPathNode m]
nodeSetToList = nodeSetMap id
