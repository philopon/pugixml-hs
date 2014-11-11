{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Text.XML.Pugi

main :: IO ()
main = defaultMain tests

testAStr :: S.ByteString
testAStr = "<?xml version=\"1.0\" lang=\"ja\"?><foo><bar baz=\"qux\">quux</bar><bar baz=\"bar\">hoge</bar><piyo><![CDATA[<piyopiyo>]]></piyo></foo><foo2 />"

testA :: Document
testA = either undefined id $ parse def { parseFlags = parseFull } testAStr

tests :: TestTree
tests = testGroup "Tests"
    [ parsePretty
    , immutable
    ]

parsePretty :: TestTree
parsePretty = testGroup "Parse/Pretty"
    [ testCase "testA" $ pp testAStr @?= L.fromStrict testAStr
    ]
  where
    pp s = either undefined (pretty def {prettyFlags = formatRaw} ) $
        parse def { parseFlags = parseFull } s

immutable :: TestTree
immutable = testGroup "Immutable"
    [ immutableTestAFoo
    ]

immutableTestAFoo :: TestTree
immutableTestAFoo = testGroup "TestA/foo"
    [ testCase "parent.child == id" $ Just node @?= (child "bar" node >>= parent)
    , testCase "prevSibling.nextSibling == id" $ Just node @?= (nextSibling node >>= prevSibling)
    , testCase "asNode == id" $ node @?= asNode node
    , testCase "getName.firstChild == \"bar\"" $ (getName <$> firstChild node) @?= Just "bar"
    , testCase "getName.lastChild == \"piyo\"" $ (getName <$> lastChild node) @?= Just "piyo"
    , testCase "path == \"/foo\"" $ path '/' node @?= "/foo"
    , testCase "root == parent" $ root node @?= parent node
    ]
  where Just node = child "foo" testA
