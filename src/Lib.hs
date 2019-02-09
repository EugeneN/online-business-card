{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE GADTs              #-}

module Lib
    ( siteComponent
    ) where

import GHCJS.Types(JSString)
import Data.String (fromString)
import Data.ByteString
import qualified Data.Text          as T  
import qualified Data.JSString                  as JSS      
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void, join)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A        
import qualified Web.VirtualDom.Html.Events     as E  
import           Lubeck.App                     (Html)
import qualified Data.ByteString.Char8          as BS

import Data.Aeson
import Data.Aeson.Types

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)
import           UICombinators
import Utils

import qualified Data.Tree as DT
import qualified Data.Tree.Zipper as Z

import Types
import Net


data ArticleStatus = ArtPending | ArtError DatasourceError | ArtReady Gist

siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  (viewU, viewModel) <- newSignal ArtPending
  (stateU, stateModel) <- newSignal Nothing :: Z.PosType t => FRP ( Sink (Maybe (DT.Forest Page, Z.TreePos t Page))
                                                                  , Signal (Maybe (DT.Forest Page, Z.TreePos t Page)))

  subscribeEvent (filterJust $ updates stateModel) $ \(f, z) -> do
    print $ Z.label z
    let gist = dataSource $ Z.label z
    print gist
    loadGist_ viewU gist $ viewU . ArtReady 

  let v = fmap view viewModel

  loadGist_ viewU (rootGist c) $ \a -> 
    case unfiles $ files a of
      [] -> viewU $ ArtError $ DatasourceError "There are no files in this forest"
      (f:fs) -> do
        let forest = eitherDecodeStrict' . BS.pack . JSS.unpack . f_content $ f :: Either String (DT.Forest Page)
        print forest
        case forest of
              Left err -> viewU $ ArtError $ DatasourceError $ JSS.pack err
              Right forest' -> case Z.nextTree (Z.fromForest forest') of
                                  Nothing -> viewU $ ArtError $ DatasourceError "There are no trees in this forest"
                                  Just x' -> do
                                    print forest'
                                    print x'
                                    stateU $ Just (forest', x')

  pure v

  where 
    loadGist_ :: Sink ArticleStatus -> GistId -> (Gist -> IO ()) -> IO ()
    loadGist_ viewU g f = void . forkIO $ do
      a <- loadGist g -- :: IO (Either DatasourceError a)
      case a of
        Left x -> viewU $ ArtError x
        Right a' -> f a'

    view ArtPending = label "Pending"
    view (ArtError (DatasourceError s)) = label s
    view (ArtReady a) = gistH $ unfiles $ files a

    gistH :: [File] -> Html
    gistH as = H.div [] (join $ fmap renderFileH as)
              
    renderFileH :: File -> [Html]
    renderFileH f = htmlStringToVirtualDom $ f_content f
      

