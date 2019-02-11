{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs              #-}

module Types where

import           GHCJS.Types(JSString)
import           JavaScript.Web.XMLHttpRequest
import qualified GHC.Generics as GHC
import           Control.Concurrent             (forkIO)
import           Control.Exception
import           Control.Monad                  (void)
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString
import           Data.JSString.Text             (textFromJSString, textToJSString)
import qualified Data.HashMap.Strict            as HM    
import           Data.Monoid                    ((<>))
import           Data.String                    (fromString)
import qualified Data.Text                      as T  
import           Data.Time                      (UTCTime)
import qualified Data.Tree                      as DT
import qualified Data.Tree.Zipper               as Z
import           System.Random

import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A        
import qualified Web.VirtualDom.Html.Events     as E  
import qualified Data.ByteString.Char8          as BS

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)

import           UICombinators
import           Utils


instance FromJSON JSString where
  parseJSON = fmap textToJSString . parseJSON

instance ToJSON JSString where
  toJSON = toJSON . textFromJSString

data SiteConfig = SiteConfig 
  { rootGist :: GistId }

newtype GistId = GistId { getGistId :: JSString }  

type Url = JSString

type Path = [Url]

data Page = Page {
    title      :: JSString
  , path       :: Url
  , dataSource :: GistId
} deriving (GHC.Generic, ToJSON, FromJSON)

instance Show Page where
  show p = "Page " <> show (title p)

data Mimetype = Plaintext | OtherMimetype JSString | UnknownMimetype JSString
                deriving (Show)

instance FromJSON Mimetype where
  parseJSON (String "text/plain") = pure Plaintext
  parseJSON (String x)            = pure $ OtherMimetype $ showJS x
  parseJSON x                     = pure $ UnknownMimetype $ showJS x

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

newtype Files = Files { unfiles :: [File]}

deriving instance Show Files

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

data Gist = 
  Gist 
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

data DatasourceError = DatasourceError Menu JSString | NotFound Menu Path | Waiting  Menu Path

deriving instance Show DatasourceError

type Menu = [[MenuItem]]

emptyMenu = []

data MenuItem = MISelected JSString Path | MIUnselected JSString Path deriving (Show)

-- blog1Page   = DT.Node (Page "Blog 1" "blog1" (GistId "?")) []
-- blog2Page   = DT.Node (Page "Blog 2" "blog2" (GistId "?")) []
-- aboutPage   = DT.Node (Page "About" "about" (GistId "b0bb1c06c091b06264f939748df0cf3a")) []
-- cvPage      = DT.Node (Page "CV" "cv" (GistId "9fa2fe92a22a1fb3da0caf735c3afbe5")) []
-- blogPage    = DT.Node (Page "Blog" "blog" (GistId "?")) [blog1Page, blog2Page]
-- photosPage  = DT.Node (Page "Photos" "photos" (GistId "?")) []
-- talksPage   = DT.Node (Page "Talks" "talks" (GistId "?")) []
-- cmdPage   = DT.Node (Page "cmdPage" "cmdPage" (GistId "?")) []
-- golPage   = DT.Node (Page "golPage" "golPage" (GistId "?")) []
-- twicPage   = DT.Node (Page "twicPage" "twicPage" (GistId "?")) []
-- mltoolPage   = DT.Node (Page "mltoolPage" "mltoolPage" (GistId "?")) []
-- g4Page   = DT.Node (Page "g4Page" "g4Page" (GistId "?")) []
-- appsPage    = DT.Node (Page "Apps" "apps" (GistId "?")) [cmdPage, golPage, twicPage, mltoolPage, g4Page]

-- thesite = [aboutPage, photosPage, cvPage, blogPage, appsPage, talksPage]

-- zip = Z.fromForest thesite 
