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


jss2text :: JSString -> T.Text
jss2text = T.pack . JSS.unpack

text2jss :: T.Text -> JSString 
text2jss = JSS.pack . T.unpack

-- TODO FIXME move to gist config
redirects :: Path -> Maybe Path
redirects ("blog":xs) = Just $ "essays":xs
redirects []          = Just ["en"]
redirects _           = Nothing

data BlogRecord = 
  BlogRecord 
    { day        :: Int
    , month      :: Int
    , year       :: Int
    , humanTitle :: JSString
    , hash       :: JSString
    , slug       :: JSString
    , isPublic   :: Bool
    } deriving (GHC.Generic, ToJSON, FromJSON)

newtype BlogIndex = 
  BlogIndex { unblog :: [BlogRecord] }
  deriving (GHC.Generic, ToJSON, FromJSON)

type BlogIndexFull = (BlogIndex, BlogGist)

data AuthKey = 
  AuthKey 
    { token    :: JSString
    , user     :: GithubUser
    }

data ViewMode = Site | Login | Editor

data Lock = Locked | Unlocked AuthKey

newtype RootGist = 
  RootGist { digout :: Gist }
  deriving (Show)

newtype BlogGist = 
  BlogGist { blogout :: Gist }
  deriving (Show)

type Model  = (Area, Path, Maybe BlogIndexFull)
type Model_ = (Area, Path)

instance FromJSON JSString where
  parseJSON = fmap textToJSString . parseJSON

instance ToJSON JSString where
  toJSON = toJSON . textFromJSString

data SiteConfig = 
  SiteConfig 
    { rootGist :: GistId
    , blogGist :: GistId 
    } deriving (GHC.Generic, ToJSON, FromJSON)

newtype GistId = 
  GistId 
    { getGistId :: JSString }  

type Url = JSString

type Path = [Url]

data Area =
  Area
    { blogSlug           :: Url
    , collapsedMenuPaths :: [Url]
    , titleRoot          :: JSString
    , titleSep           :: JSString
    , forest             :: DT.Forest Page
    , hiddenForest       :: DT.Forest Page
    } deriving (GHC.Generic, ToJSON, FromJSON, Show)

emptyArea :: Area
emptyArea = Area "" [] "" "" [] []   

data Page = 
  Page 
    { title        :: JSString
    , path         :: Url
    , dataSource   :: GistId -- Gist | IPFS | ...
    , isSpecial    :: Bool
    -- , processor :: Html | BlogIndex | Etc
    } deriving (GHC.Generic, ToJSON, FromJSON)

instance Show Page where
  show p = "Page " <> show (title p)

data Mimetype = 
    Plaintext 
  | OtherMimetype JSString 
  | UnknownMimetype JSString
  deriving (Show)

instance ToJSON Mimetype where
  toJSON Plaintext           = "text/plain"
  toJSON (OtherMimetype x)   = String . jss2text $ x
  toJSON (UnknownMimetype x) = String . jss2text $ x

instance FromJSON Mimetype where
  parseJSON (String "text/plain") = pure Plaintext
  parseJSON (String x)            = pure $ OtherMimetype $ showJS x
  parseJSON x                     = pure $ UnknownMimetype $ showJS x

instance FromJSON GistId where
  parseJSON (String x) = pure . GistId . text2jss $ x
  parseJSON _          = mzero

instance ToJSON GistId where
    toJSON (GistId x) = toJSON x

instance FromJSON Files where
  parseJSON (Object x) = Files <$> mapM parseJSON (HM.elems x)
  parseJSON _          = mzero

instance ToJSON Files where
  toJSON (Files [])    = object [ ]
  toJSON (Files (x:_)) = object [ jss2text (f_filename x) .= toJSON x ]

instance FromJSON File where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = Prelude.drop $ Prelude.length ("f_" :: String)}

instance ToJSON File where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop $ length ("f_" :: String)}

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
    , public      :: Bool
    , html_url    :: Url
    } deriving (GHC.Generic, FromJSON, ToJSON, Show)

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
gistApi = API "https://api.github.com/gists" []

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
    MISelected   JSString Path Bool -- title path isSpecial 
  | MIUnselected JSString Path Bool
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
    -- , gu_private_gists             :: Int 
    -- , gu_total_private_repos       :: Int 
    -- , gu_owned_private_repos       :: Int 
    -- , gu_disk_usage                :: Int 
    -- , gu_collaborators             :: Int 
    -- , gu_two_factor_authentication :: Bool 
    } deriving (GHC.Generic)  

instance FromJSON GithubUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = Prelude.drop $ Prelude.length ("gu_" :: String)}

-- blog1Page   = DT.Node (Page "Blog 1" "blog1" (GistId "?")) []
-- blog2Page   = DT.Node (Page "Blog 2" "blog2" (GistId "?")) []
-- aboutPage   = DT.Node (Page "About" "about" (GistId "b0bb1c06c091b06264f939748df0cf3a")) []
-- cvPage      = DT.Node (Page "CV" "cv" (GistId "9fa2fe92a22a1fb3da0caf735c3afbe5")) []
-- blogPage    = DT.Node (Page "Essays" "essays" (GistId "?")) [blog1Page, blog2Page]
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

-- bi :: BlogIndex
-- bi = 
--   BlogIndex 
--     [ BlogRecord 19 2  2019 "Mathematics"                                             "367afcbe6e16201c56c053e825392c87" "mathematics"
--     , BlogRecord 18 2  2019 "meta.repl 2 - The revenge of WYSIWYG"                    "d78d9cd2ebd8c670606152762b623620" "meta.repl-2-the-revenge-of-wysiwyg"
--     , BlogRecord 18 2  2019 "meta.repl 1"                                             "3ad14ea7bf1dce157a058dd82b7ff659" "meta.repl-1"
--     , BlogRecord 14 2  2019 "IoT "                                                    "4af23c6300868d85edf79faf743032fe" "iot"
--     , BlogRecord 14 2  2019 "Why “worse” things work "                                "8f92c0638d7084f23a6bfe6d18aeea2e" "why-worse-things-work"
--     , BlogRecord 12 9  2018 "Me-oriented travel service "                             "103aaf342540f0562360fd2d33d8befe" "me-oriented-travel-service"
--     , BlogRecord 31 12 2018 "Technology is a multiplier "                             "a47499bb7ada12c7879c435aa74e7293" "technology-is-a-multiplier"
--     , BlogRecord 12 2  2017 "Why React is so popular "                                "f2ab0c730792262e9f820075c4ba4012" "why-react-is-so-popular"
--     , BlogRecord 9 30  2017 "Project 8 "                                              "5b4e7d908cb94ecf301c6cf733c9d65f" "project-8"
--     , BlogRecord 1 28  2017 "Multi-level programming language "                       "3cf4a90656fe6622631d19334f0f028a" "multi-level-programming-language"
--     , BlogRecord 1 11  2017 "Many languages of UI "                                   "b5004d2f750b47682e5fe76e49406165" "many-languages-of-ui"
--     , BlogRecord 1 4   2017 "Microservices "                                          "95e98991640e5afcc3d7eb692ada6de9" "microservices"
--     , BlogRecord 12 13 2016 "Obvious way to build a  web  API "                       "6e0e9a49a3d7990399170a33fa6d132e" "obvious-way-to-build-a-web-api"
--     , BlogRecord 11 10 2016 "Building a large, complex web application with Haskell " "a9a51ff2c20b6b8aaa2d87208fd45b6d" "building-a-large,-complex-web-application-with-haskell"
--     , BlogRecord 8 8   2016 "Software Engineering "                                   "2969d9fecd518337115638821679160f" "software-engineering"
--     , BlogRecord 7 6   2016 "HTML user input and virtual dom "                        "6e6cd20902f0eac3087374b34cdd453c" "html-user-input-and-virtual-dom"
--     , BlogRecord 6 30  2016 "Atom + Haskell "                                         "a6591ac9a5c2deb60f34a2ea568f1863" "atom-and-haskell"
--     , BlogRecord 2 17  2016 "Divide and rule "                                        "5431761c6544b261be05"             "divide-and-rule"
--     , BlogRecord 3 9   2016 "Framework as type "                                      "9cd87f04072cf70bbbee"             "framework-as-type"
--     , BlogRecord 1 29  2016 "meta.repl "                                              "1db636e4abdb59a615d3"             "meta.repl"
--     , BlogRecord 1 26  2016 "OOP "                                                    "f06430e57f16ceb62c53"             "oop"
--     , BlogRecord 1 25  2016 "MVVM Component "                                         "d3b9d9f579a32798b85c"             "mvvm-component"
--     , BlogRecord 1 23  2016 "Stateful components "                                    "ddb4287df6585105018e"             "stateful-components"
--     , BlogRecord 1 18  2016 "Ladder of Abstraction "                                  "827a94c6a1a0fec3801e"             "ladder-of-abstraction"
--     , BlogRecord 1 17  2016 "TDD "                                                    "14f062aa73fbc19b1d8f"             "tdd"
--     , BlogRecord 1 16  2016 "Strong vs Unityped "                                     "3901963f724f90190a1c"             "strong-vs-unityped"
--     , BlogRecord 1 14  2016 "Constrain everything "                                   "a5980063201c3b8c2112"             "constrain-everything"
--     , BlogRecord 1 13  2016 "Immutable world "                                        "2b7285f5fc7f8ccd2e07"             "immutable-world"
--     , BlogRecord 12 30 2015 "Abstract (\"Web\") Application "                         "c1d71ff4e670ef82c535"             "abstract-web-application"
--     , BlogRecord 12 17 2015 "DNA Architecture "                                       "cdc388425dacf87cba71"             "dna-architecture"
--     , BlogRecord 12 21 2015 "ActivitiesView.js "                                      "9a374a884cf2a06d0f45"             "activitiesview.js"
--     , BlogRecord 1 24  2016 "Test "                                                   "8f4d22137a20da9237b9"             "test"
--     ]