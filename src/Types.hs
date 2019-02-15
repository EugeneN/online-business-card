{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Types where

import           GHCJS.Types(JSString)
import qualified GHC.Generics as GHC
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable                  (asum)
import qualified Data.JSString                  as JSS  
import           Data.JSString.Text             (textFromJSString, textToJSString)
import qualified Data.HashMap.Strict            as HM    
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Data.Time                      (UTCTime)
import qualified Data.Tree                      as DT

import           Lubeck.Util                    (showJS)


data AuthKey = 
  AuthKey 
    { username :: JSString
    , password :: JSString
    , user     :: GithubUser
    }

data ViewMode = Site | Login

data Lock = Locked | Unlocked AuthKey

type Model = (DT.Forest Page, Path, Lock)
type Model_ = (DT.Forest Page, Path)

instance FromJSON JSString where
  parseJSON = fmap textToJSString . parseJSON

instance ToJSON JSString where
  toJSON = toJSON . textFromJSString

data SiteConfig = 
  SiteConfig 
    { rootGist :: GistId }

newtype GistId = 
  GistId 
    { getGistId :: JSString }  

type Url = JSString

type Path = [Url]

data Page = 
  Page 
    { title      :: JSString
    , path       :: Url
    , dataSource :: GistId
    } deriving (GHC.Generic, ToJSON, FromJSON)

instance Show Page where
  show p = "Page " <> show (title p)

data Mimetype = 
    Plaintext 
  | OtherMimetype JSString 
  | UnknownMimetype JSString
  deriving (Show)

instance FromJSON Mimetype where
  parseJSON (String "text/plain") = pure Plaintext
  parseJSON (String x)            = pure $ OtherMimetype $ showJS x
  parseJSON x                     = pure $ UnknownMimetype $ showJS x

instance FromJSON GistId where
  parseJSON (String x) = pure . GistId . JSS.pack . T.unpack $ x
  parseJSON _          = mzero

instance ToJSON GistId where
    toJSON (GistId x) = toJSON x

instance FromJSON Files where
  parseJSON (Object x) = Files <$> mapM parseJSON (HM.elems x)
  parseJSON _          = mzero

instance FromJSON File where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = Prelude.drop $ Prelude.length ("f_" :: String)}

newtype Files = 
  Files 
    { unfiles :: [File]}

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

type ApiResult = Either Message Gist

data Message = 
  Message 
    { message           :: JSString
    , documentation_url :: Maybe JSString
    } deriving (GHC.Generic, FromJSON, Show)

instance FromJSON ApiResult where
  parseJSON o@(Object _) =  
    asum [ Right <$> (parseJSON o :: Parser Gist)
         , Left <$> (parseJSON o :: Parser Message)
         ]   
    
  parseJSON x = typeMismatch "Gist" x

type Header = (JSString, JSString) 

data API = 
  API 
    { baseURL :: JSString
    , headers :: [Header]
    }

gistApi :: API
gistApi = API "https://api.github.com/gists/" []

userApi :: API
userApi = API "https://api.github.com/user" []

data DatasourceError = 
    DatasourceError JSString 
  | NotFound Path 

deriving instance Show DatasourceError

newtype MenuLevel = 
  MenuLevel 
    { unlevel :: [MenuItem] }
  deriving (Show)

data Menu = 
    Menu MenuLevel Menu 
  | MenuNil
  deriving (Show)

emptyMenu :: Menu
emptyMenu = Menu (MenuLevel []) MenuNil

data MenuItem = 
    MISelected JSString Path 
  | MIUnselected JSString Path 
  deriving (Show)

data GithubUser = 
  GithubUser
    { gu_login                     :: JSString 
    , gu_id                        :: Int 
    , gu_avatar_url                :: Url 
    , gu_url                       :: Url 
    , gu_html_url                  :: Url 
    , gu_followers_url             :: Url 
    , gu_following_url             :: Url 
    , gu_gists_url                 :: Url 
    , gu_starred_url               :: Url 
    , gu_subscriptions_url         :: Url 
    , gu_organizations_url         :: Url 
    , gu_repos_url                 :: Url 
    , gu_events_url                :: Url 
    , gu_received_events_url       :: Url 
    , gu_name                      :: JSString 
    , gu_public_repos              :: Int 
    , gu_public_gists              :: Int 
    , gu_followers                 :: Int 
    , gu_following                 :: Int 
    , gu_created_at                :: UTCTime 
    , gu_updated_at                :: UTCTime 
    , gu_private_gists             :: Int 
    , gu_total_private_repos       :: Int 
    , gu_owned_private_repos       :: Int 
    , gu_disk_usage                :: Int 
    , gu_collaborators             :: Int 
    , gu_two_factor_authentication :: Bool 
    } deriving (GHC.Generic)  

instance FromJSON GithubUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = Prelude.drop $ Prelude.length ("gu_" :: String)}

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
