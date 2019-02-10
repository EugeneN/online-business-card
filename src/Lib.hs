{-# LANGUAGE OverloadedStrings          #-}
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


data GistStatus = GistPending | GistError DatasourceError | GistReady Gist

siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  (viewU, viewModel) <- newSignal GistPending
  (stateU, stateModel) <- newSignal Nothing :: Z.PosType t => FRP ( Sink (Maybe (DT.Forest Page, Z.TreePos t Page))
                                                                  , Signal (Maybe (DT.Forest Page, Z.TreePos t Page)))

  subscribeEvent (filterJust $ updates stateModel) $ \(f, z) -> do
    print $ Z.label z
    let gist = dataSource $ Z.label z
    print gist
    loadGist_ viewU gist $ viewU . GistReady 

  let v = fmap view viewModel

  loadGist_ viewU (rootGist c) $ \a -> 
    case unfiles $ files a of
      [] -> viewU $ GistError $ DatasourceError "There are no files in this forest"
      (f:fs) -> do
        let forest = eitherDecodeStrict' . BS.pack . JSS.unpack . f_content $ f :: Either String (DT.Forest Page)
        print forest
        case forest of
              Left err -> viewU $ GistError $ DatasourceError $ JSS.pack err
              Right forest' -> case Z.nextTree (Z.fromForest forest') of
                                  Nothing -> viewU $ GistError $ DatasourceError "There are no trees in this forest"
                                  Just x' -> do
                                    print forest'
                                    print x'
                                    stateU $ Just (forest', x')

  pure v

  where 
    loadGist_ :: Sink GistStatus -> GistId -> (Gist -> IO ()) -> IO ()
    loadGist_ viewU g f = void . forkIO $ do
      a <- loadGist g -- :: IO (Either DatasourceError a)
      case a of
        Left x -> viewU $ GistError x
        Right a' -> f a'

    view GistPending = H.div [A.class_ "loader-container"] 
                             [ H.img [A.class_ "ajax-loader", A.src "img/ajax-loader.gif"] []
                             , H.text "Loading" ]
    view (GistError (DatasourceError s)) = label s
    view (GistReady a) = gistH $ unfiles $ files a

    gistH :: [File] -> Html
    gistH as = H.div [] (join $ fmap renderFileH as)
              
    renderFileH :: File -> [Html]
    renderFileH f = htmlStringToVirtualDom $ f_content f
      

