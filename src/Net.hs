{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs              #-}

module Net
    ( loadGist
    , getAPI
    , patchAPI
    , postAPI
    , saveGist
    ) where

import           GHCJS.Types                   (JSString, JSVal)
import           GHCJS.Marshal                 (toJSVal_aeson)
import           JavaScript.Web.XMLHttpRequest
import           Data.Aeson
import           Data.ByteString
import qualified Data.ByteString.Char8          as C8
import qualified Data.JSString                  as JSS   
import           Data.Monoid                   ((<>))
import           Data.String                   (fromString)
import           Control.Exception
import           Control.Monad.Except
import           System.Random

import           Types


loadGist :: FromJSON a => GistId -> IO (Either DatasourceError a)
loadGist x = getAPI gistApi ("/" <> getGistId x)

saveGist :: (FromJSON b) => Gist -> IO (Either DatasourceError b)
saveGist x = patchAPI gistApi ("/" <> getGistId (Types.id x)) x

mkAPIpath :: API -> Url -> IO Url
mkAPIpath api pathSuffix = do
  g <- newStdGen
  let salt = fromString . Prelude.take 20 $ randomRs ('a', 'z') g
  return $ canonicalUrl <> querySep <> salt

  where
    canonicalUrl = baseURL api <> pathSuffix
    querySep     = case JSS.findIndex (== '?') canonicalUrl of
                     Nothing -> "?"
                     _       -> "&"

xhrWithCredentials :: Bool                     
xhrWithCredentials = False

bsToJss :: ByteString -> JSString
bsToJss = JSS.pack . C8.unpack

xhrerrorToJss :: XHRError -> JSString
xhrerrorToJss = JSS.pack . show

getAPI :: (FromJSON a, Monad m, MonadIO m) 
       => API -> JSString -> m (Either DatasourceError a)
getAPI api pathSuffix = do
  requestURI   <- liftIO $ mkAPIpath api pathSuffix
  eitherResult <- liftIO (try $ xhrByteString (request requestURI) :: IO (Either XHRError (Response ByteString)))
  case eitherResult of
    Left s       -> pure . Left . DatasourceError . xhrerrorToJss $ s
    Right result -> case contents result of
      Nothing          -> pure . Left $ DatasourceError "getAPI: No response"
      Just byteString  -> case Data.Aeson.eitherDecodeStrict' byteString of
        Left err -> pure . Left . DatasourceError $ "getAPI: Parse error " <> JSS.pack err <> " in " <> bsToJss byteString
        Right x  -> pure $ Right x
  where
    request requestURI = Request { reqMethod          = GET
                                 , reqURI             = requestURI
                                 , reqLogin           = Nothing
                                 , reqHeaders         = headers api
                                 , reqWithCredentials = xhrWithCredentials
                                 , reqData            = NoData
                                 }

patchAPI :: (ToJSON a, FromJSON b, Monad m, MonadIO m) 
         => API -> JSString -> a -> m (Either DatasourceError b)
patchAPI api pathSuffix value = do
  requestURI   <- liftIO $ mkAPIpath api pathSuffix
  body         <- liftIO $ encodeJSString value
  eitherResult <- liftIO (try $ xhrByteString (request requestURI body) :: IO (Either XHRError (Response ByteString)))
  case eitherResult of
    Left s       -> pure . Left . DatasourceError . xhrerrorToJss $ s
    Right result -> case contents result of
      Nothing          -> pure . Left $ DatasourceError "patchAPI: No response"
      Just byteString  -> case Data.Aeson.eitherDecodeStrict' byteString of
        Left err -> pure . Left . DatasourceError $ "patchAPI: Parse error " <> JSS.pack err <> " in " <> bsToJss byteString
        Right x  -> pure $ Right x
  where
    request requestURI body = Request { reqMethod          = PATCH
                                      , reqURI             = requestURI
                                      , reqLogin           = Nothing
                                      , reqHeaders         = headers api
                                      , reqWithCredentials = xhrWithCredentials
                                      , reqData            = StringData body
                                      }                                 

postAPI :: (ToJSON a, FromJSON b, Monad m, MonadIO m) 
         => API -> JSString -> a -> m (Either DatasourceError b)
postAPI api pathSuffix value = do
  requestURI   <- liftIO $ mkAPIpath api pathSuffix
  body         <- liftIO $ encodeJSString value
  eitherResult <- liftIO (try $ xhrByteString (request requestURI body) :: IO (Either XHRError (Response ByteString)))
  case eitherResult of
    Left s       -> pure . Left . DatasourceError . xhrerrorToJss $ s
    Right result -> case contents result of
      Nothing          -> pure . Left $ DatasourceError "postAPI: No response"
      Just byteString  -> case Data.Aeson.eitherDecodeStrict' byteString of
        Left err -> pure . Left . DatasourceError $ "postAPI: Parse error " <> JSS.pack err <> " in " <> bsToJss byteString
        Right x  -> pure $ Right x
  where
    request requestURI body = Request { reqMethod          = POST
                                      , reqURI             = requestURI
                                      , reqLogin           = Nothing
                                      , reqHeaders         = headers api
                                      , reqWithCredentials = xhrWithCredentials
                                      , reqData            = StringData body
                                      }                                 

encodeJSString :: ToJSON a => a -> IO JSString
encodeJSString value = do
  jsval <- toJSVal_aeson value
  return $ stringify jsval
-- Use toJSVal_aeson to get a JSVal
-- Pass this to a JSON.stringify wrapper  

foreign import javascript unsafe "JSON.stringify($1)"
  stringify :: JSVal -> JSString                                 