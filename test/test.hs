{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Text.XML.Pugi
import Data.Maybe

main :: IO ()
main = defaultMain tests

testAStr :: S.ByteString
testAStr = "<?xml version=\"1.0\" lang=\"ja\"?><foo><bar baz=\"qux\">quux</bar><bar baz=\"bar\">hoge</bar><piyo><![CDATA[<piyopiyo>]]></piyo></foo>"

testA :: Document
testA = either undefined id $ parse def { parseFlags = parseFull } testAStr

tests :: TestTree
tests = testGroup "Tests"
    [ parsePretty
    ]

parsePretty :: TestTree
parsePretty = testGroup "Parse/Pretty"
    [ testCase "testA" $ pp testAStr @?= L.fromStrict testAStr
    ]
  where
    pp s = either undefined (pretty def {prettyFlags = formatRaw} ) $
        parse def { parseFlags = parseFull } s

