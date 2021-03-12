{-# LANGUAGE LambdaCase #-}
module Server.Handler.Users
  ( signUpH
  , signInH
  , signOutH
  )
where

import           Database.Database
import           Database.User.User as U
import           Servant
import           Server.Types       as S
import           Server.Types.Util  (dbToServerError)

import           Relude

signUpH :: S.User -> HandlerT NoContent
signUpH S.User{..} = do
  tbs <- asks tables
  res <- liftIO . runDB tbs . createUser $ U.User userEmail [] userPassword
  case res of
    Right _ -> return NoContent
    Left  e -> throwError $ dbToServerError e

signInH :: S.User -> HandlerT Token
signInH S.User{..} = do
  tbs <- asks tables
  res <- liftIO . runDB tbs $ getUser userEmail
  case res of
    Right (Just u) -> return $ Token "kek"
    Right Nothing  -> throwError $ err404 { errBody = "User not found" }
    Left  e        -> throwError $ dbToServerError e

signOutH :: HandlerT NoContent
signOutH = do
  return NoContent
