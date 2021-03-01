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


    itdb "insert url with nil orig url" $ \tables -> do
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


    itdb "insert url with non-existing user" $ \tables -> do
      let shortUrl = "https://localhost/r/tenr"
          url = Url "https://ro-che.info/ccc/27" shortUrl
          expected = orig url

      -- insert without inserting user
      got  <- left  =<< run tables (insertUrl url "none")
      got `shouldBe` EUserNExist


    itdb "insert already existing url" $ \tables -> do
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


  -- describe "delete url" do
  --   itdb "delete url after inserting it" $ \tables -> do
  --     let uname = "ling10394"
  --         user  = User uname [] "IEktv8j$nk2;3U"
  --         shortUrl = "https://localhost/r/OEl"
  --         url = Url "https://haskell.org" shortUrl

  --     -- insert user, url and query it
  --     resU  <- right =<< run tables (insertUser user)
  --     resIn <- right =<< run tables (insertUrl url uname)
  --     resQ  <- right =<< run tables (queryUrl shortUrl)
  --     -- delete url and make sure it's gone
  --     resD  <- right =<< run tables (deleteUrl shortUrl uname)
  --     got   <- right =<< run tables (queryUrl shortUrl)

  --     got `shouldBe` Nothing

  --   itdb "delete non-existing url" $ \tables -> do
  --     let uname = "ling10394"
  --         user  = User uname [] "IEktv8j$nk2;3U"
  --         shortUrl = "https://localhost/r/OEl"

  --     -- insert user, and try to delete url
  --     resU <- right =<< run tables (insertUser user)
  --     got  <- left  =<< run tables (deleteUrl shortUrl uname)

  --     got `shouldBe` ESUrlNExist


  describe "uuid actions" do
    itdb "update uuid" $ \tables -> do
      res <- right =<< run tables genUUID
      let expected = res + 1
      got <- right =<< run tables genUUID
      got `shouldBe` expected
