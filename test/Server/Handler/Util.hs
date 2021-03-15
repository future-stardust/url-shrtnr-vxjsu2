module Server.Handler.Util where

import Relude
import Servant

userName :: Text
userName = "test@test.com"

userPass :: Text
userPass = "testpasswd"

testUrl :: String
testUrl = "http://localhost"

dbFileName :: String
dbFileName = "test_db"

basicAuthData :: BasicAuthData
basicAuthData = BasicAuthData (encodeUtf8 userName) (encodeUtf8 userPass)