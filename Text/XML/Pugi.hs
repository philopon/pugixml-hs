{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
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
    , N.NodeLike
      -- ** getter
    , hashValue , nodeType
    , getName, getValue
    , parent, firstChild, lastChild, nextSiling, prevSiling
    , child, attribute
    , nextSiblingByName, prevSiblingByName
    , findChildByNameAndAttr, findChildByAttr
    , childValue, childValueByName, text
    , N.mapSiblingM, N.mapSiblingM_, mapSibling
    , N.mapAttrsM, N.mapAttrsM_, mapAttrs
    , path, firstElementByPath, root
    , selectSingleNode, selectNodes

    -- ** setter
    , Modify
    , create, modify
    , setName, setValue
    , appendAttr, prependAttr
    , appendChild, prependChild
    , appendCopy, prependCopy
    , removeAttr, removeChild
    , appendParse

    -- * XPath
    , NodeSet, XPathNode, Attribute, XPath
    , X.nodeSetSize
    , nodeSetIndex
    , X.nodeSetMapM, X.nodeSetMapM_, nodeSetMap
    , X.EvalXPath
    , X.xpath, evaluate

    -- * reexport
    , module Text.XML.Pugi.Const
    , def
    ) where

import Control.Applicative

import Foreign.C.Types

import           Text.XML.Pugi.Const
import           Text.XML.Pugi.Foreign.Types
import qualified Text.XML.Pugi.Foreign.Document as D
import qualified Text.XML.Pugi.Foreign.Node as N
import qualified Text.XML.Pugi.Foreign.XPath      as X
import qualified Text.XML.Pugi.Foreign.XPath.Node as X

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Default.Class

import System.IO.Unsafe

parse :: D.ParseConfig -> S.ByteString
      -> Either D.ParseException Document
parse cfg str = unsafePerformIO $ D.parse cfg str

pretty :: D.PrettyConfig -> Document -> L.ByteString
pretty cfg doc = unsafePerformIO $ D.pretty cfg doc

hashValue :: N.NodeLike n => n m -> CSize
hashValue = unsafePerformIO . N.hashValue

nodeType :: N.NodeLike n => n m -> NodeType
nodeType = unsafePerformIO . N.nodeType

getName :: N.NodeLike n => n m -> S.ByteString
getName = unsafePerformIO . N.getName

getValue :: N.NodeLike n => n m -> S.ByteString
getValue = unsafePerformIO . N.getValue

parent :: N.NodeLike n => n m -> Maybe (Node_ m)
parent = unsafePerformIO . N.parent

firstChild :: N.NodeLike n => n m -> Maybe (Node_ m)
firstChild = unsafePerformIO . N.firstChild

lastChild :: N.NodeLike n => n m -> Maybe (Node_ m)
lastChild = unsafePerformIO . N.lastChild

nextSiling :: N.NodeLike n => n m -> Maybe (Node_ m)
nextSiling = unsafePerformIO . N.nextSibling

prevSiling :: N.NodeLike n => n m -> Maybe (Node_ m)
prevSiling = unsafePerformIO . N.prevSibling

child :: N.NodeLike n => S.ByteString -> n m -> Maybe (Node_ m)
child n = unsafePerformIO . N.child n

attribute :: N.NodeLike n => S.ByteString -> n m -> Maybe S.ByteString
attribute n = unsafePerformIO . N.attribute n

nextSiblingByName :: N.NodeLike n => S.ByteString -> n m -> Maybe (Node_ m)
nextSiblingByName n = unsafePerformIO . N.nextSiblingByName n

prevSiblingByName :: N.NodeLike n => S.ByteString -> n m -> Maybe (Node_ m)
prevSiblingByName n = unsafePerformIO . N.prevSiblingByName n

findChildByNameAndAttr :: N.NodeLike n
                       => S.ByteString -- ^ node name
                       -> S.ByteString -- ^ attribute name
                       -> S.ByteString -- ^ attribute value
                       -> n m -> Maybe (Node_ m)
findChildByNameAndAttr nn an av =
    unsafePerformIO . N.findChildByNameAndAttr nn an av

findChildByAttr :: N.NodeLike n
                => S.ByteString -- ^ attribute name
                -> S.ByteString -- ^ attribute value
                -> n m -> Maybe (Node_ m)
findChildByAttr an av =
    unsafePerformIO . N.findChildByAttr an av

childValue :: N.NodeLike n => n m -> S.ByteString
childValue = unsafePerformIO . N.childValue

childValueByName :: N.NodeLike n => S.ByteString -> n m -> S.ByteString
childValueByName n = unsafePerformIO . N.childValueByName n

text :: N.NodeLike n => n m -> S.ByteString
text = unsafePerformIO . N.text

mapSibling :: N.NodeLike n => (Node_ m -> a) -> n m -> [a]
mapSibling f = unsafePerformIO . N.mapSiblingM (return . f)

mapAttrs :: N.NodeLike n => (S.ByteString -> S.ByteString -> a) -> n m -> [a]
mapAttrs f = unsafePerformIO . N.mapAttrsM (\k v -> return $ f k v)

path :: N.NodeLike n => Char -> n m -> S.ByteString
path c = unsafePerformIO . N.path c

firstElementByPath :: N.NodeLike n => Char -> S.ByteString -> n m -> Maybe (Node_ m)
firstElementByPath c p = unsafePerformIO . N.firstElementByPath c p

root :: N.NodeLike n => n m -> Maybe (Node_ m)
root = unsafePerformIO . N.root

newtype Modify a = Modify { unModify :: IO a }
    deriving (Functor, Applicative, Monad)

create :: (MutableDocument -> Modify ()) -> Document
create m = D.freezeDocument . unsafePerformIO . unModify $ do
    d <- Modify D.createDocument
    m d
    return d

modify :: Document -> (MutableDocument -> Modify ()) -> Document
modify prt m = D.freezeDocument . unsafePerformIO . unModify $ do
    d <- Modify $ D.copyDocument prt
    m d
    return d

setName :: N.NodeLike n => S.ByteString -> n Mutable -> Modify Bool
setName n = Modify . N.setName n

setValue :: N.NodeLike n => S.ByteString -> n Mutable -> Modify Bool
setValue n = Modify . N.setValue n

appendAttr :: N.NodeLike n => S.ByteString -> S.ByteString
           -> n Mutable -> Modify Bool
appendAttr k v = Modify . N.appendAttr k v

prependAttr :: N.NodeLike n => S.ByteString -> S.ByteString
            -> n Mutable -> Modify Bool
prependAttr k v = Modify . N.prependAttr k v

appendChild :: N.NodeLike n => NodeType -> n Mutable -> Modify (Maybe MutableNode)
appendChild t = Modify . N.appendChild t

prependChild :: N.NodeLike n => NodeType -> n Mutable -> Modify (Maybe MutableNode)
prependChild t = Modify . N.prependChild t

appendCopy :: N.NodeLike n => Node_ a -> n Mutable -> Modify (Maybe MutableNode)
appendCopy t = Modify . N.appendCopy t

prependCopy :: N.NodeLike n => Node_ a -> n Mutable -> Modify (Maybe MutableNode)
prependCopy t = Modify . N.prependCopy t

removeAttr :: N.NodeLike n => S.ByteString -> n Mutable -> Modify Bool
removeAttr n = Modify . N.removeAttr n

removeChild :: N.NodeLike n => Node_ a -> n Mutable -> Modify Bool
removeChild n = Modify . N.removeChild n

appendParse :: N.NodeLike n => D.ParseConfig -> S.ByteString -> n Mutable -> Modify Bool
appendParse cfg str = Modify . N.appendBuffer cfg str

selectSingleNode :: N.NodeLike n => XPath NodeSet -> n m -> XPathNode
selectSingleNode x = unsafePerformIO . N.selectSingleNode x

selectNodes :: N.NodeLike n => XPath NodeSet -> n m -> NodeSet
selectNodes x = unsafePerformIO . N.selectNodes x

nodeSetIndex :: NodeSet -> Int -> XPathNode
nodeSetIndex n = unsafePerformIO . X.nodeSetIndex n

nodeSetMap :: (XPathNode -> a) -> NodeSet -> [a]
nodeSetMap f = unsafePerformIO . X.nodeSetMapM (return . f)

evaluate :: (N.NodeLike n, X.EvalXPath r) => XPath r -> n m -> r
evaluate x = unsafePerformIO . X.evaluateXPath x
