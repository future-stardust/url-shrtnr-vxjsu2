module Server.UtilSpec (spec) where

import Test.Hspec

import Relude

import Server.Util

spec :: Spec
spec = do
  describe "shortenWithAlphabet" $ do
    it "https://kek.lol/arbidol" $ do
      let got = shortenWithAlphabet alphabet 228
      got `shouldBe` "3G" -- all things as they should be (pun intended)
