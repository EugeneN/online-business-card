{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE GADTs              #-}

module Lib
    ( siteComponent
    , SiteConfig(..)
    , GistId(..)
    ) where

import           Data.Monoid                    ((<>))
import GHCJS.Types(JSString)
import JavaScript.Web.XMLHttpRequest
import Control.Exception
import Control.Monad.Except
import Data.String (fromString)
import           Data.Time                            (UTCTime)
import System.Random
import qualified Data.JSString
import Data.ByteString
import qualified GHC.Generics as GHC
import           Data.JSString.Text (textFromJSString, textToJSString)

import Data.Aeson
import Data.Aeson.Types

import           Lubeck.App                     (Html)
import           Lubeck.Forms.Select
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)
import qualified Components.Map                 as Map
import           UICombinators


instance FromJSON JSString where
  parseJSON = fmap textToJSString . parseJSON

data SiteConfig = SiteConfig 
        { rootGist :: GistId }

newtype GistId = GistId { getGistId :: JSString }        

siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  a <- loadGist . rootGist $ c
  pure $ pure $ label $ showJS a

data Error = Error JSString

data Mimetype = Plaintext | UnknownMimetype JSString
                deriving (Show)


instance FromJSON Mimetype where
  parseJSON (String "text/plain") = pure Plaintext
  parseJSON (String x) = pure $ UnknownMimetype $ showJS x

newtype Files = Files [File]

deriving instance FromJSON Files
deriving instance Show Files
deriving instance GHC.Generic Files

deriving instance FromJSON GistId
deriving instance Show GistId
deriving instance GHC.Generic GistId

data File = 
  File 
    { content  :: JSString
    , filename     :: JSString
    , language     :: JSString
    , size     :: Int
    , mimetype :: Mimetype 
    } deriving (GHC.Generic, FromJSON, Show)


data Article = 
  Article 
    { updated_at  :: Maybe UTCTime
    , created_at  :: Maybe UTCTime
    , id          :: GistId
    , description :: JSString
    , files       :: Files 
    } deriving (GHC.Generic, FromJSON, Show)

type Header = (JSString, JSString) 

data API = API {
    baseURL :: JSString
  , headers :: [Header]
}
   

gistApi =   API "https://api.github.com/gists/" []

loadGist :: GistId -> IO (Either DatasourceError Article)
loadGist x = getAPI gistApi (getGistId x)

mkAPIpath api path = do
  g <- newStdGen
  let salt = fromString . Prelude.take 20 $ randomRs ('a', 'z') g
  return $ canonicalUrl <> querySep <> salt

  where
    canonicalUrl = baseURL api <> path
    querySep     = case Data.JSString.findIndex (== '?') canonicalUrl of
                     Nothing -> "?"
                     _       -> "&"

xhrWithCredentials = False

newtype DatasourceError = DatasourceError JSString

deriving instance Show DatasourceError

getAPI :: (FromJSON a, Monad m, s ~ JSString, MonadIO m) 
       => API -> JSString -> m (Either DatasourceError a)
getAPI api path = do
  requestURI <- liftIO $ mkAPIpath api path
  eitherResult <- liftIO (try $ xhrByteString (request requestURI) :: IO (Either XHRError (Response ByteString)) )
  case eitherResult of
    Left s       -> pure $ Left $ DatasourceError $ showJS s
    Right result -> case contents result of
      Nothing          -> pure $ Left $ DatasourceError "getAPI': No response"
      Just byteString  -> case Data.Aeson.decodeStrict byteString of
        Nothing -> pure $ Left $ DatasourceError $ "getAPI: Parse error " <> showJS byteString
        Just x  -> pure $ Right x
  where
    request requestURI = Request { reqMethod          = GET
                                 , reqURI             = requestURI
                                 , reqLogin           = Nothing
                                 , reqHeaders         = headers api
                                 , reqWithCredentials = xhrWithCredentials
                                 , reqData            = NoData
                                 }