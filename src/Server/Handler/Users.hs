{-# LANGUAGE DataKinds #-}

module Server.Handler.Users
  ( signUpH
  , signInH
  , signOutH
  )
where

import           Servant
import           Servant.Auth.Server

import           Relude

import           Database.Database
import           Database.User.User  as U
import           Server.Auth
import           Server.Types        as T
import           Server.Types.Util   (dbToServerError)
import           Server.Util

signUpH :: T.User -> HandlerT NoContent
signUpH T.User{..} = do
  log <- asks logger
  logWith log $ "User " <> userEmail <> " is trying to sign up"

  tbs <- asks tables
  res <- liftIO . runDB tbs . createUser $ U.User userEmail [] userPassword
  case res of
    Right _ -> do
      logWith log $ "User " <> userEmail <> " successfuly signed up"
      return NoContent
    Left  e -> do
      logWith log $ "Got an error: " <> show e
      throwError $ dbToServerError e

signInH :: CookieSettings -> JWTSettings -> T.User -> HandlerT AuthNoContent
signInH cookies jwts u@T.User{..} = do
  log <- asks logger
  logWith log $ "User " <> userEmail <> " is trying to sign in"

  tbs <- asks tables
  res <- liftIO . runDB tbs $ getUser userEmail
  void $ case res of
           Right (Just u') -> if hash u' == userPassword then return () else throwError err404
           _               -> throwError err404
  appCookies <- liftIO $ acceptLogin cookies jwts u
  case appCookies of
    Nothing -> do
      logWith log "Sign in failed"
      throwError err404
    Just c  -> do
      logWith log "Successful sign in"
      return $ c NoContent

signOutH :: CookieSettings -> AuthResult T.User -> HandlerT AuthNoContent
signOutH cookies (Authenticated _) = return $ clearSession cookies NoContent
signOutH _       _                 = throwError err401
