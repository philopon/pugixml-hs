{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

-- | import/langauge
--
-- @
-- \> :set -XOverloadedStrings -XQuasiQuotes
-- \> import Text.XML.Pugi
-- \> import qualified Text.XML.Pugi.Mutable as M
-- @
--
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
-- testHtml = M.create $ \doc -\> do
--     decl <- M.appendDeclaration \"xml\" doc
--     M.appendAttrs [(\"version\", \"1.0\"), (\"lang\", \"ja\")] decl
--
--     M.appendDoctype \"html\" doc
--
--     html <- M.appendElement \"html\" doc
--     body <- M.appendElement \"body\" html
--     div_ <- M.appendElement \"div\"  body
--     a    <- M.appendElement \"a\"    div_
--     M.appendAttr \"href\" \"http:\/\/example.com\" a
--     txt  <- M.appendPCData \"example.com\" a
--     return ()
-- @
--
-- @
-- -- testHtml for copy&paste to ghci.
-- \> doc \<- M.create $ \\doc -\> M.appendDeclaration \"xml\" doc >>= \\decl -\> M.appendAttrs [(\"version\", \"1.0\"), (\"lang\", \"ja\")] decl >> M.appendDoctype \"html\" doc >> M.appendElement \"html\" doc >>= \\html -\> M.appendElement \"body\" html >>= \\body -\> M.appendElement \"div\"  body >>= \\div_ -\> M.appendElement \"a\"    div_ >>= \\a -\> M.appendAttr \"href\" \"http:\/\/example.com\" a >> M.appendPCData \"example.com\" a >> return ()
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
-- M.modify doc $ \\d -\> M.selectSingleNode [xpath|\/\/a|] d >>= \\(Left a) -\> M.setOrAppendAttr \"href\" \"#\" a
-- Document \<?xml version=\"1.0\" lang=\"ja\"?\>\<!DOCTYPE html\>\<html\>\<body\>\<div\>\<a href=\"#\"\>example.com\<\/a\>\<\/div\>\<\/body\>\<\/html\>
-- @
-- 

module Text.XML.Pugi
    ( -- * Document
      Document_, Document, MutableDocument, MutableFlag(..)
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
    , asMutable, asImmutable

    -- ** getter
    , NodeLike(..)

    -- * XPath
    , XPath
#if __GLASGOW_HASKELL__ > 707
    , X.EvalXPath(X.XPathResult)
#else
    , X.EvalXPath(..)
#endif
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

class NodeLike n where
    asNode                 :: n k Immutable -> Node_ k Immutable
    nodeEqual              :: n k Immutable -> n l Immutable -> Bool
    forgetNodeKind         :: n k Immutable -> n Unknown Immutable
    forgetNodeKind = unsafeCoerce
    {-# INLINE forgetNodeKind #-}
    prettyNode             :: D.PrettyConfig -> Int -> n k Immutable -> L.ByteString
    hashValue              :: n k Immutable -> CSize
    nodeType               :: n k Immutable -> NodeType
    getName                :: HasName  k => n k Immutable -> S.ByteString
    getValue               :: HasValue k => n k Immutable -> S.ByteString
    parent                 :: n k Immutable -> Maybe Node
    firstChild             :: HasChildren k => n k Immutable -> Maybe Node
    lastChild              :: HasChildren k => n k Immutable -> Maybe Node
    nextSibling            :: n k Immutable -> Maybe Node
    prevSibling            :: n k Immutable -> Maybe Node
    child                  :: HasChildren  k => S.ByteString -> n k Immutable -> Maybe Node
    attribute              :: HasAttribute k => S.ByteString -> n k Immutable -> Maybe S.ByteString
    nextSiblingByName      :: S.ByteString -> n k Immutable -> Maybe Node
    prevSiblingByName      :: S.ByteString -> n k Immutable -> Maybe Node
    findChildByNameAndAttr :: HasChildren k
                           => S.ByteString -- ^ node name
                           -> S.ByteString -- ^ attribute name
                           -> S.ByteString -- ^ attribute value
                           -> n k Immutable -> Maybe Node
    findChildByAttr        :: HasChildren k
                           => S.ByteString -- ^ attribute name
                           -> S.ByteString -- ^ attribute value
                           -> n k Immutable -> Maybe Node
    childValue             :: HasChildren k => n k Immutable -> S.ByteString
    childValueByName       :: HasChildren k => S.ByteString -> n k Immutable -> S.ByteString
    text                   :: n k Immutable -> S.ByteString

    -- | find attribute by predicate. since v0.2.0.
    findAttribute          :: (S.ByteString -> S.ByteString -> Bool) -> n k Immutable -> Maybe Attribute

    -- | find child by predicate. since v0.2.0.
    findChild              :: (Node -> Bool) -> n k Immutable -> Maybe Node

    -- | find node by predicate. since v0.2.0.
    findNode               :: (Node -> Bool) -> n k Immutable -> Maybe Node
    mapSibling             :: (Node_ Unknown Immutable -> a) -> n k Immutable -> [a]
    mapAttrs               :: HasAttribute k => (S.ByteString -> S.ByteString -> a) -> n k Immutable -> [a]
    path                   :: Char -> n k Immutable -> S.ByteString
    firstElementByPath     :: Char -> S.ByteString -> n k Immutable -> Maybe Node
    root                   :: n k Immutable -> Maybe Node
    evaluate               :: X.EvalXPath r => XPath r -> n k Immutable -> (X.XPathResult r Immutable)
    selectSingleNode       :: XPath NodeSet -> n k Immutable -> XPathNode Immutable
    selectNodes            :: XPath NodeSet -> n k Immutable -> NodeSet Immutable

instance NodeLike Document_ where
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

instance NodeLike Node_ where
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

nodeSetIndex :: NodeSet m -> Int -> XPathNode m
nodeSetIndex n = unsafeDupablePerformIO . X.nodeSetIndex n

nodeSetMap :: (XPathNode m -> a) -> NodeSet m -> [a]
nodeSetMap f = unsafeDupablePerformIO . X.nodeSetMapM (return . f)

nodeSetToList :: NodeSet m -> [XPathNode m]
nodeSetToList = nodeSetMap id

asMutable :: N.NodeLike n => n k Mutable -> n k Mutable
asMutable = id
{-# INLINE asMutable #-}

asImmutable :: N.NodeLike n => n k Immutable -> n k Immutable
asImmutable = id
{-# INLINE asImmutable #-}
