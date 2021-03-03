module Database.Url.UrlDBSpec (spec) where

import Relude
import Relude.Unsafe (fromJust)
import Test.Hspec

import Database.Test.Aux
import Database.Common
import Database.Url.Url
import Database.Url.UrlDB
import Database.User.User
import Database.User.UserDB

spec :: Spec
spec = do
  describe "insertUrl" do
    itdb "insert url and then query it" $ \tables -> do
      let uname = "bartosz"
          user = User uname [] "IEktv8j$nk2;3U"
          shortUrl = "https://localhost/r/abc"
          url = Url "https://bartoszmilewski.com" shortUrl
          expected = orig url
      -- insert user, url, and then query it
      resU  <- right =<< run tables (insertUser user)
      resIn <- right =<< run tables (insertUrl url uname)
      resQ  <- right <$> run tables (queryUrl shortUrl)
      -- should return original url
      got   <- fromJust <$> resQ
      got `shouldBe` expected


    itdb "insert url with nil short and orig urls" $ \tables -> do
      let uname = "bartosz498"
          user = User uname [] "IEktv8j$nk2;3U"
          shortUrl = ""
          url = Url "" shortUrl
          expected = orig url
      -- insert user, url, and then query it
      resI <- right =<< run tables (insertUser user)
      got  <- left  =<< run tables (insertUrl url uname)
      -- should return original url
      got `shouldBe` EUrlNil


    itdb "insert url with nil orig url fail" $ \tables -> do
      let uname = "curry909"
          user = User uname [] "IEktv8j$nk2;3U"
          -- shortUrl = "https://ro-che.info/ccc/10"
          shortUrl = "https://localhost/r/b5g4"
          url = Url "" shortUrl
          expected = orig url

      -- insert user and url
      resI <- right =<< run tables (insertUser user)
      got  <- left  =<< run tables (insertUrl url uname)
      got `shouldBe` EUrlNil


    itdb "insert url with nil short url" $ \tables -> do
      let uname = "onCurrency"
          user = User uname [] "IEktv8j$nk2;3U"
          shortUrl = ""
          url = Url "https://ro-che.info/ccc/27" shortUrl
          expected = orig url

      -- insert user and url
      resI <- right =<< run tables (insertUser user)
      got  <- left  =<< run tables (insertUrl url uname)
      got `shouldBe` EUrlNil


    itdb "insert url with non-existing user fail" $ \tables -> do
      let shortUrl = "https://localhost/r/tenr"
          url = Url "https://ro-che.info/ccc/27" shortUrl
          expected = orig url

      -- insert without inserting user
      got  <- left  =<< run tables (insertUrl url "none")
      got `shouldBe` EUserNExist


    itdb "insert already existing url fail" $ \tables -> do
      let uname = "lof90"
          user = User uname [] "IEktv8j$nk2;3U"
          shortUrl = "https://localhost/r/OEl"
          url = Url "https://haskell.org" shortUrl
          expected = orig url

      -- insert user, url, make sure it exists
      -- then try to insert it again
      resU  <- right =<< run tables (insertUser user)
      resIn <- right =<< run tables (insertUrl url uname)
      resQ  <- right =<< run tables (queryUrl shortUrl)
      got   <- left  =<< run tables (insertUrl url uname)

      got `shouldBe` EUrlExist



  describe "query url" do
    itdb "query url after inserting it" $ \tables -> do
      let uname = "lof90"
          user  = User uname [] "IEktv8j$nk2;3U"
          shortUrl = "https://localhost/r/OEl"
          url = Url "https://haskell.org" shortUrl
          expected = orig url

      -- insert user, url and then try query it
      resU  <- right =<< run tables (insertUser user)
      resIn <- right =<< run tables (insertUrl url uname)
      resQ  <- right <$> run tables (queryUrl shortUrl)
      -- should return original url
      got <- fromJust <$> resQ
      got `shouldBe` expected


    itdb "query non-existing url" $ \tables -> do
      let shortUrl = "https://localhost/r/1Tlk7"

      -- try to query non-existing url
      got <- right =<< run tables (queryUrl shortUrl)
      got `shouldBe` Nothing


  describe "delete url" do
    itdb "delete url after inserting it" $ \tables -> do
      let uname = "ling10394"
          user  = User uname [] "IEktv8j$nk2;3U"
          shortUrl = "https://localhost/r/OEl"
          url = Url "https://haskell.org" shortUrl

      -- insert user, url and query it
      resU  <- right =<< run tables (insertUser user)
      resIn <- right =<< run tables (insertUrl url uname)
      resQ  <- right =<< run tables (queryUrl shortUrl)
      -- delete url and make sure it's gone
      resD  <- right =<< run tables (deleteUrl shortUrl uname)
      got   <- right =<< run tables (queryUrl shortUrl)

      got `shouldBe` Nothing


    itdb "delete non-existing url fail" $ \tables -> do
      let uname = "baktkgn5382"
          user  = User uname [] "IEktv8j$nk2;3U"
          shortUrl = "https://localhost/r/nonexisting"

      -- insert user, and try to delete url
      resU <- right =<< run tables (insertUser user)
      got  <- left  =<< run tables (deleteUrl shortUrl uname)

      got `shouldBe` EUrlNExist


    itdb "delete empty url fail" $ \tables -> do
      let uname = "blackkkk"
          shortUrl = ""

      -- try to delete url
      got <- left  =<< run tables (deleteUrl shortUrl uname)
      got `shouldBe` ESUrlNil


    itdb "delete url for non-existing user fail" $ \tables -> do
      let unameNExist = "nonexisting"
          uname = "eric999"
          user  = User uname [] "IEktv8j$nk2;3U"
          shortUrl = "https://localhost/r/TLGn8"
          url = Url "https://golem.ph.utexas.edu" shortUrl

      -- insert user and url
      resU <- right =<< run tables (insertUser user)
      resI <- right =<< run tables (insertUrl url uname)
      -- try to delete existing url, but from another user
      got  <- left  =<< run tables (deleteUrl shortUrl unameNExist)
      got `shouldBe` EUserNExist



  describe "uuid actions" do
    itdb "update uuid" $ \tables -> do
      -- generate uuid
      expected <- right =<< run tables genUUID
      -- generate uuid once more
      got      <- right =<< run tables genUUID
      -- the result should be 1 more than the previous one
      got `shouldBe` (expected + 1)
