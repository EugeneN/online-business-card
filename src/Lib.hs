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
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A        
import qualified Web.VirtualDom.Html.Events     as E  
import           Lubeck.App                     (Html)

import Data.Aeson
import Data.Aeson.Types

import           Lubeck.App                     (Html)
import           Lubeck.Forms.Select
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)
import qualified Components.Map                 as Map
import           UICombinators
import Utils

import qualified Data.Tree as DT
import qualified Data.Tree.Zipper as Z


instance FromJSON JSString where
  parseJSON = fmap textToJSString . parseJSON

instance ToJSON JSString where
  toJSON = toJSON . textFromJSString

data SiteConfig = SiteConfig 
        { rootGist :: GistId }

newtype GistId = GistId { getGistId :: JSString }  

data ArticleStatus = ArtPending | ArtError DatasourceError | ArtReady Article

siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  (u, model) <- newSignal ArtPending

  -- print $ DT.drawForest $ fmap (fmap show) thesite
  let a = encode thesite
  print a
  let b = eitherDecode' a :: Either String (DT.Forest Page)
  print $ show b

  let v = fmap view model
  void . forkIO $ do
    a <- loadGist . rootGist $ c
    case a of
      Left x -> u $ ArtError x
      Right a' -> u $ ArtReady a'

  pure v

  where 
    view ArtPending = label "Pending"
    view (ArtError (DatasourceError s)) = label s
    view (ArtReady a) = gistH (getFiles $ files a)

    gistH :: [File] -> Html
    gistH as = H.div [] (join $ fmap renderFileH as)
              
    renderFileH :: File -> [Html]
    renderFileH f = htmlStringToVirtualDom $ f_content f
      

--------------------------------------------------------------------------------
type Url = JSString

data Page = Page {
    title      :: JSString
  , path       :: Url
  , dataSource :: GistId
} deriving (GHC.Generic, ToJSON, FromJSON)

instance Show Page where
  show p = "Page " <> show (title p)

blog1Page   = DT.Node (Page "Blog 1" "blog1" (GistId "?")) []
blog2Page   = DT.Node (Page "Blog 2" "blog2" (GistId "?")) []
aboutPage   = DT.Node (Page "About" "about" (GistId "b0bb1c06c091b06264f939748df0cf3a")) []
cvPage      = DT.Node (Page "CV" "cv" (GistId "9fa2fe92a22a1fb3da0caf735c3afbe5")) []
blogPage    = DT.Node (Page "Blog" "blog" (GistId "?")) [blog1Page, blog2Page]
photosPage  = DT.Node (Page "Photos" "photos" (GistId "?")) []
talksPage   = DT.Node (Page "Talks" "talks" (GistId "?")) []
cmdPage   = DT.Node (Page "cmdPage" "cmdPage" (GistId "?")) []
golPage   = DT.Node (Page "golPage" "golPage" (GistId "?")) []
twicPage   = DT.Node (Page "twicPage" "twicPage" (GistId "?")) []
mltoolPage   = DT.Node (Page "mltoolPage" "mltoolPage" (GistId "?")) []
g4Page   = DT.Node (Page "g4Page" "g4Page" (GistId "?")) []
appsPage    = DT.Node (Page "Apps" "apps" (GistId "?")) [cmdPage, golPage, twicPage, mltoolPage, g4Page]

thesite = [aboutPage, photosPage, cvPage, blogPage, appsPage, talksPage]

zip = Z.fromForest thesite 

--------------------------------------------------------------------------------

data Mimetype = Plaintext | OtherMimetype JSString | UnknownMimetype JSString
                deriving (Show)

instance FromJSON Mimetype where
  parseJSON (String "text/plain") = pure Plaintext
  parseJSON (String x) = pure $ OtherMimetype $ showJS x
  parseJSON x          = pure $ UnknownMimetype $ showJS x

instance FromJSON GistId where
  parseJSON (String x) = pure . GistId . text2jss $ x
  parseJSON x          = mzero

instance ToJSON GistId where
    toJSON (GistId x) = toJSON x

instance FromJSON Files where
  parseJSON (Object x) = Files <$> mapM parseJSON (HM.elems x)
  parseJSON x          = mzero

instance FromJSON File where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = Prelude.drop $ Prelude.length ("f_" :: String)}

newtype Files = Files { getFiles :: [File]}

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