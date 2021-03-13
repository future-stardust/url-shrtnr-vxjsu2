{-# LANGUAGE DataKinds  #-}

module Server.Handler.Users
  ( signUpH
  , signInH
  , signOutH
  )
where

import           Database.Database
import           Database.User.User  as U
import           Servant
import           Server.Types        as T
import           Server.Types.Util   (dbToServerError)

import           Relude
import           Servant.Auth.Server

signUpH :: T.User -> HandlerT NoContent
signUpH T.User{..} = do
  tbs <- asks tables
  res <- liftIO . runDB tbs . createUser $ U.User userEmail [] userPassword
  case res of
    Right _ -> return NoContent
    Left  e -> throwError $ dbToServerError e

signInH :: CookieSettings -> JWTSettings -> T.User -> HandlerT (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
signInH cookies jwts u@T.User{..} = do
  tbs <- asks tables
  res <- liftIO . runDB tbs $ getUser userEmail
  void $ case res of
           Right (Just _) -> return ()
           _              -> throwError err404
  appCookies <- liftIO $ acceptLogin cookies jwts u
  case appCookies of
    Nothing -> throwError err404
    Just c  -> return $ c NoContent

signOutH :: CookieSettings -> AuthResult T.User -> HandlerT (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
signOutH cookies (Authenticated _) = return $ clearSession cookies NoContent
signOutH _       _                 = throwError err401
