module Database.User.UserSpec (spec) where

import Data.SafeCopy
import Data.Serialize
import Relude
import Test.Hspec

import           Database.Common
import qualified Database.Tree.Tree as BT
import           Database.User.User

spec :: Spec
spec = do
  describe "Eq User" do
    it "==" do
      let user1 = User "user" [] "thYJfub$*8"
          user2 = User "user" [] "tehkhtl"
      user1 `shouldBe` user2

    it "/=" do
      let user1 = User "user1" [] "lielTKN"
          user2 = User "user2" [] "lielTKN"
      user1 `shouldNotBe` user2



  describe "SafeCopy User" do
    it "safePut & safeGet" do
      let expected = User "foobar"
                          [ "https://test.url"
                          , "https://haskell.org"
                          ] "bar"
      -- encode, decode and compare result with the source
      let encoded  = runPut $ safePut expected
          decodedE = runGet  (safeGet :: Get User) encoded
          got = case decodedE of
                  Right u -> u
                  Left  e -> error $ toText e

      got `shouldBe` expected



  describe "SafeCopy TreeUser" do
    it "safePut & safeGet" do
      let user = User "foobar"
                      [ "https://test.url"
                      , "https://haskell.org"
                      ] "bar"
          tree = BT.singleton user :: TreeUser
          expected = BT.toList tree

      -- encode, decode and compare result with the source
      let encoded  = runPut $ safePut tree
          decodedE = runGet  (safeGet :: Get TreeUser) encoded
          decoded  = case decodedE of
                       Right u -> u
                       Left  e -> error $ toText e
          got = BT.toList decoded

      got `shouldBe` expected
