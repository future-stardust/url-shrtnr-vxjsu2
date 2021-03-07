module Database.User.UserDBSpec (spec) where


import Database.Common
import Database.User.User
import Database.User.UserDB
import Database.Database hiding (deleteUrl, genUUID)
import Database.Test.Aux

import Data.Maybe (fromJust)
import Relude hiding (empty)
import Test.Hspec


spec :: Spec
spec = do
  describe "insertUser && queryUser" do
    itdb "insertUser then query it" $ \tables -> do
      let expected = User "William" [] "SEoinXKenoi@5!@'a"

      -- insert user and check that there's no error (matches Right)
      _   <- right =<< runDB tables (insertUser expected)
      -- and then query it
      res <- right <$> runDB tables (queryUser "William")
      got <- fromJust <$> res
      got `shouldBe` expected


    itdb "zero values username and hash in User fail" $ \tables -> do
      let user = User "" [] ""
      -- try to insert user
      got <- left =<< runDB tables (insertUser user)
      -- should return an exception
      got `shouldBe` EUserNil


    itdb "zero value username fail" $ \tables -> do
      let user = User "" [] "ioUBknil.t42#tKL"
      -- try to insert user
      got <- left =<< runDB tables (insertUser user)
      -- should return an exception
      got `shouldBe` EUserNil


    itdb "zero value hash fail" $ \tables -> do
      let user = User "notnil9" [] ""
      -- try to insert user
      got <- left =<< runDB tables (insertUser user)
      -- should return an exception
      got `shouldBe` EUserNil


    itdb "username exists fail" $ \tables -> do
      let user1 = User "Bob15" [] "Sihoten5$%e"
          user2 = User "Bob15" [] "iEOsihths%#$@^sht"

      -- try to insert users one after the other
      _   <- right =<< runDB tables (insertUser user1)
      got <- left  =<< runDB tables (insertUser user2)
      -- should return an exception
      got `shouldBe` EUserExist



  describe "updateUrlsUser" do
    itdb "add url to User's empty list of urls" $ \tables -> do
      let uname = "Zach588"
          urls  = []
          url   = "https://github.com"
      let user     = User uname urls "SihekthsV"
          expected = user{ urls=urls <> [url] }

      -- insert user, then update it and query for result
      _   <- right <$> runDB tables (insertUser user)
      _   <- right <$> runDB tables (updateUrlsUser uname url)
      res <- right <$> runDB tables (queryUser uname)
      -- should return user with a new url
      got <- fromJust <$> res
      got `shouldBe` expected


    itdb "add url to User's list of urls" $ \tables -> do
      let uname = "Zach588"
          urls  = ["https://duckduckgo.com", "https://haskell.org"]
          url   = "https://github.com"
      let user     = User uname urls "SihekthsV"
          expected = user{ urls=urls <> [url]}

      -- insert user, then update it and query for result
      _   <- right <$> runDB tables (insertUser user)
      _   <- right <$> runDB tables (updateUrlsUser uname url)
      res <- right <$> runDB tables (queryUser uname)
      -- should return user with a new url
      got <- fromJust <$> res
      got `shouldBe` expected


    itdb "add url to User that doesn't exist fail" $ \tables -> do
        let uname = "nonExistingUser"
            url   = "https://github.com"

        -- try to update `User`
        got <- left =<< runDB tables (updateUrlsUser uname url)
        -- should return user with a new url
        got `shouldBe` EUserNExist


    itdb "add url to User with nil username fail" $ \tables -> do
        let uname = ""
            url   = "https://github.com"

        -- try to update `User`
        got <- left =<< runDB tables (updateUrlsUser uname url)
        -- should return user with a new url
        got `shouldBe` EUserNExist


    itdb "add nil url fail" $ \tables -> do
        let uname = "User4345"
            urls  = ["https://duckduckgo.com", "https://haskell.org"]
            url   = ""
        let user     = User uname urls "SihekthsV"

        -- insert a user, then try update it
        _   <- right =<< runDB tables (insertUser user)
        got <- left  =<< runDB tables (updateUrlsUser uname url)
        got `shouldBe` ESUrlNil


    itdb "add existing url fail" $ \tables -> do
        let uname = "User4345"
            urls  = ["https://duckduckgo.com", "https://haskell.org"]
            url   = "https://haskell.org"
        let user     = User uname urls "SihekthsV"

        -- insert a user, then try update it
        _   <- right =<< runDB tables (insertUser user)
        got <- left  =<< runDB tables (updateUrlsUser uname url)
        got `shouldBe` ESUrlExist



  describe "queryUser" do
    itdb "query user after insert" $ \tables -> do
      let uname    = "Pablo"
          user     = User uname [] "ThaHTg"
          expected = user

      -- insert a user, then try to query it
      _   <- right =<< runDB tables (insertUser user)
      res <- right <$> runDB tables (queryUser uname)
      -- should return the same user
      got <- fromJust <$> res
      got `shouldBe` expected


    itdb "query user which doesn't exist fail" $ \tables -> do
      -- try to query user which doesn't exist in db
      got <- right =<< runDB tables (queryUser "nonexistinguser")
      got `shouldBe` Nothing


    itdb "query user with nil username fail" $ \tables -> do
      -- try to query user which doesn't exist in db
      got <- right =<< runDB tables (queryUser "")
      got `shouldBe` Nothing




  describe "queryUserUrls" do
    itdb "query urls" $ \tables -> do
      let uname = "deus498"
          urls  = ["https://foldr.com"]
          user  = User uname urls "1oTi^Ip%uelkoIUbkv%#"
          expected = urls

      -- insert a user, then try to query his urls
      _   <- right =<< runDB tables (insertUser user)
      res <- right <$> runDB tables (queryUserUrls uname)
      -- should return the same urls
      got <- fromJust <$> res
      got `shouldBe` expected


    itdb "query urls nil" $ \tables -> do
      let uname = "deus498"
          urls  = []
          user  = User uname urls "1oTi^Ip%uelkoIUbkv%#"
          expected = urls

      -- insert a user, then try to query his urls
      _   <- right =<< runDB tables (insertUser user)
      res <- right <$> runDB tables (queryUserUrls uname)
      -- should return the same urls
      got <- fromJust <$> res
      got `shouldBe` expected


    itdb "query urls with user which doesn't exist fail" $ \tables -> do
      -- try to query urls with user which doesn't exist in db
      got <- right =<< runDB tables (queryUser "nonexistinguser")
      got `shouldBe` Nothing


    itdb "query urls with user with nil username fail" $ \tables -> do
      -- try to query urls with user which doesn't exist in db
      got <- right =<< runDB tables (queryUser "")
      got `shouldBe` Nothing



  describe "deleteUrlUser" do
    itdb "delete url from User" $ \tables -> do
      let uname = "yngwie245"
          urls  = ["https://foldr.com", "https://gitlab.com"]
          urlToDel = "https://gitlab.com"
          user  = User uname urls "1oTi^Ip%uelkoIUbkv%#"
          expected = filter (/= urlToDel) urls

      -- insert a user, delete one of urls, then try to query his urls
      _   <- right =<< runDB tables (insertUser user)
      _   <- right =<< runDB tables (deleteUrlUser uname urlToDel)
      res <- right <$> runDB tables (queryUserUrls uname)
      -- should return the same user, but without certain url
      got <- fromJust <$> res
      got `shouldBe` expected

    itdb "delete url from non-existing user fail" $ \tables -> do
      let url = "https://godbolt.org"
      -- try to delete one of user's urls
      got <- left =<< runDB tables (deleteUrlUser "none" url)
      got `shouldBe` EUserNExist

    itdb "delete non-existing url fail" $ \tables -> do
      let uname = "ryan"
          urls  = ["https://foldr.com", "https://gitlab.com"]
          urlToDel = "https://tryhaskell.org"
          user  = User uname urls "1oTi^Ip%uelkoIUbkv%#"

      -- insert a user, delete one of urls, then try to query his urls
      _   <- right =<< runDB tables (insertUser user)
      got <- left  =<< runDB tables (deleteUrlUser uname urlToDel)
      got `shouldBe` ESUrlNExist

    itdb "delete nil-value url fail" $ \tables -> do
      let uname = "delamer4in"
          urls  = ["https://foldr.com", "https://gitlab.com"]
          urlToDel = ""
          user  = User uname urls "1oTi^Ip%uelkoIUbkv%#"

      -- insert a user, delete one of urls, then try to query his urls
      _   <- right =<< runDB tables (insertUser user)
      got <- left  =<< runDB tables (deleteUrlUser uname urlToDel)
      got `shouldBe` ESUrlNil
