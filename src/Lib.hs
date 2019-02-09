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
import qualified Data.Text          as T  
import qualified Data.JSString                  as JSS      
import qualified Data.HashMap.Strict as HM    
import Data.Monoid ((<>))

import Data.Aeson
import Data.Aeson.Types

import           Lubeck.App                     (Html)
import           Lubeck.Forms.Select
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)
import qualified Components.Map                 as Map
import           UICombinators

jss2text = T.pack . JSS.unpack
text2jss = JSS.pack . T.unpack

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

data Mimetype = Plaintext | OtherMimetype JSString | UnknownMimetype JSString
                deriving (Show)


instance FromJSON Mimetype where
  parseJSON (String "text/plain") = pure Plaintext
  parseJSON (String x) = pure $ OtherMimetype $ showJS x
  parseJSON x          = pure $ UnknownMimetype $ showJS x

instance FromJSON GistId where
  parseJSON (String x) = pure . GistId . text2jss $ x
  parseJSON x          = mzero

instance FromJSON Files where
  parseJSON (Object x) = Files <$> mapM parseJSON (HM.elems x)
  parseJSON x          = mzero

instance FromJSON File where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = Prelude.drop $ Prelude.length ("f_" :: String)}

newtype Files = Files [File]

deriving instance Show Files
deriving instance GHC.Generic Files

deriving instance Show GistId
deriving instance GHC.Generic GistId

data File = 
  File 
    { f_content  :: JSString
    , f_filename :: JSString
    , f_language :: JSString
    , f_size     :: Int
    , f_type     :: Mimetype 
    } deriving (GHC.Generic, Show)


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
      Just byteString  -> case Data.Aeson.eitherDecodeStrict' byteString of
        Left err -> pure $ Left $ DatasourceError $ "getAPI: Parse error " <> showJS err <> " in " <> showJS byteString
        Right x  -> pure $ Right x
  where
    request requestURI = Request { reqMethod          = GET
                                 , reqURI             = requestURI
                                 , reqLogin           = Nothing
                                 , reqHeaders         = headers api
                                 , reqWithCredentials = xhrWithCredentials
                                 , reqData            = NoData
                                 }