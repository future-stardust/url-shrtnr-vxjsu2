module Database.User.UserSpec (spec) where

import Relude
import Test.Hspec

import Database.User.User

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

  describe "Ord User" do
    it "<= eq" do
      let user1 = User "user" [] "thYJfub$*8"
          user2 = User "user" [] "tehkhtl"
      user1 `shouldSatisfy` (<= user2)

    it "<= lt" do
      let user1 = User "user1" [] "thYJfub$*8"
          user2 = User "user2" [] "tehkhtl"
      user1 `shouldSatisfy` (<= user2)

    it "<" do
      let user1 = User "user1" [] "thYJfub$*8"
          user2 = User "user2" [] "tehkhtl"
      user1 `shouldSatisfy` (< user2)

    it ">" do
      let user1 = User "user2" [] "thYJfub$*8"
          user2 = User "user1" [] "tehkhtl"
      user1 `shouldSatisfy` (> user2)

