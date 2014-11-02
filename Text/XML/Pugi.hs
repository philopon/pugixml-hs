{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
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

mLiftIO :: IO a -> Modify a
mLiftIO io = Modify $ Right <$> io

create :: Monad m => (MutableDocument -> Modify ()) -> m Document
create m = either fail (return . D.freezeDocument) . unsafePerformIO . runModify $ do
    d <- mLiftIO D.createDocument
    m d
    return d

modify :: Monad m => Document -> (MutableDocument -> Modify ()) -> m Document
modify prt m = either fail (return . D.freezeDocument) . unsafePerformIO . runModify $ do
    d <- mLiftIO $ D.copyDocument prt
    m d
    return d

setName :: N.NodeLike n => S.ByteString -> n Mutable -> Modify ()
setName n nd = mLiftIO (N.setName n nd) >>= 
    flip unless (fail $ "setName: " ++ show n)

setValue :: N.NodeLike n => S.ByteString -> n Mutable -> Modify ()
setValue n nd = mLiftIO (N.setValue n nd) >>=
    flip unless (fail $ "setValue: " ++ show n)

appendAttr :: N.NodeLike n => S.ByteString -> S.ByteString
           -> n Mutable -> Modify ()
appendAttr k v n = mLiftIO (N.appendAttr k v n) >>=
    flip unless (fail $ "appendAttr: " ++ show k ++ " = " ++ show v)

prependAttr :: N.NodeLike n => S.ByteString -> S.ByteString
            -> n Mutable -> Modify ()
prependAttr k v n = mLiftIO (N.prependAttr k v n) >>=
    flip unless (fail $ "appendAttr: " ++ show k ++ " = " ++ show v)

appendChild :: N.NodeLike n => NodeType -> n Mutable -> Modify MutableNode
appendChild t n = mLiftIO (N.appendChild t n) >>=
    maybe (fail $ "appendChild: " ++ show t) return

prependChild :: N.NodeLike n => NodeType -> n Mutable -> Modify MutableNode
prependChild t n = mLiftIO (N.prependChild t n) >>=
    maybe (fail $ "prependChild: " ++ show t) return

appendCopy :: N.NodeLike n => Node_ a -> n Mutable -> Modify MutableNode
appendCopy t n = mLiftIO (N.appendCopy t n) >>=
    maybe (fail "appendCopy") return

prependCopy :: N.NodeLike n => Node_ a -> n Mutable -> Modify MutableNode
prependCopy t n = mLiftIO (N.prependCopy t n) >>=
    maybe (fail "prependCopy") return

removeAttr :: N.NodeLike n => S.ByteString -> n Mutable -> Modify ()
removeAttr n nd = mLiftIO (N.removeAttr n nd) >>=
    flip unless (fail $ "removeAttr: " ++ show n)

removeChild :: N.NodeLike n => Node_ a -> n Mutable -> Modify ()
removeChild n nd = mLiftIO (N.removeChild n nd) >>=
    flip unless (fail "removeChild")

appendParse :: N.NodeLike n => D.ParseConfig -> S.ByteString -> n Mutable -> Modify ()
appendParse cfg str n = mLiftIO (N.appendBuffer cfg str n) >>=
    flip unless (fail $ "appendParse: " ++ show str)

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
