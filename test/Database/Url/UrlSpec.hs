module Database.Url.UrlSpec where

import Data.SafeCopy
import Data.Serialize
import Relude
import Test.Hspec

import           Database.Common
import qualified Database.Tree.Tree as BT
import           Database.Url.Url

spec :: Spec
spec = do
  describe "SafeCopy Url" do
    it "safePut & safeGet" do
      let expected = Url "foo" "bar"
      -- encode, decode and compare result with the source
      let encoded  = runPut $ safePut expected
          decodedE = runGet  (safeGet :: Get Url) encoded
          got = case decodedE of
                  Right u -> u
                  Left  e -> error $ toText e

      got `shouldBe` expected


  describe "SafeCopy TreeUrl" do
    it "safePut & safeGet" do
      let url      = Url "foo" "bar"
          tree     = BT.singleton url :: TreeUrl
          expected = BT.toList tree

      let encoded  = runPut $ safePut tree
          decodedE = runGet  (safeGet :: Get TreeUrl) encoded
          decoded  = case decodedE of
                       Right u -> u
                       Left  e -> error $ toText e
          got = BT.toList decoded

      got `shouldBe` expected
