module Server.Auth where

import           Servant
import           Servant.Auth.Server as SAS

import           Relude

import           Database.Database
import           Database.User.User  as U
import           Server.Types        as T

authCheck :: Tables -> BasicAuthData -> IO (AuthResult T.User)
authCheck tbs (BasicAuthData name passw) = do
  user <- liftIO . runDB tbs . getUser $ decodeUtf8 name
  case user of
    Right (Just u) -> if U.hash u == decodeUtf8 passw
                      then return . Authenticated $ T.User (U.username u) (U.hash u)
                      else return SAS.BadPassword
    _              -> return SAS.Indefinite

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult T.User)

instance FromBasicAuthData T.User where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData
