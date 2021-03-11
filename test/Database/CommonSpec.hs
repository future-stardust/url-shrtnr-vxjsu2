module Database.CommonSpec (spec) where

import Relude hiding (empty)
import Test.Hspec

import Database.Common

spec :: Spec
spec = do
  describe "common" do
    it "shortUrlIsNil empty url" do
      let got = shortUrlIsNil ""
      got `shouldBe` True

    it "shortUrlIsNil non-empty url" do
      let got = shortUrlIsNil "https://lichess.org"
      got `shouldBe` False
