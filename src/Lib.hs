{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Lib
    ( siteComponent
    ) where

import GHCJS.Types (JSString, JSVal)
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
import           GHCJS.Foreign.Callback

import Data.Aeson
import Data.Aeson.Types

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)
import           Lubeck.Web.URI                 (decodeURIComponent)
import           UICombinators
import Utils

import qualified Data.Tree as DT
import qualified Data.Tree.Zipper as Z

import Types
import Net


data GistStatus = GistPending | GistError DatasourceError | GistReady Gist

siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  navS <- navComp 
  (viewU, viewModel) <- newSignal GistPending
  (stateU, stateModel) <- newSignal [] :: FRP (Sink (DT.Forest Page), Signal (DT.Forest Page))

  let stateModel' = (,) <$> stateModel <*> navS :: Signal (DT.Forest Page, Path)                                                 

  subscribeEvent (updates stateModel') $ \(f, p) -> do
    print f
    print p
    case findTree f p of
      Nothing -> print "path not found in the forest" >> (viewU . GistError . DatasourceError $ "Can't find the path in the forest")
      Just page -> loadGist_ viewU (dataSource page) $ viewU . GistReady 

  let v = fmap view viewModel

  loadGist_ viewU (rootGist c) $ \a -> 
    case unfiles $ files a of
      [] -> viewU $ GistError $ DatasourceError "There are no files in this forest"
      (f:fs) -> do
        let forest = eitherDecodeStrict' . BS.pack . JSS.unpack . f_content $ f :: Either String (DT.Forest Page)
        case forest of
              Left err -> viewU $ GistError $ DatasourceError $ JSS.pack err
              Right forest' -> stateU forest'

  pure v

  where 
    findTree :: DT.Forest Page -> Path -> Maybe Page
    findTree f p = case (f, p) of
      ([],_)       -> Nothing
      (x:xs, [])   -> Just $ DT.rootLabel x
      (xs, y:ys) -> let z = Prelude.filter ((y ==) . path . DT.rootLabel) xs
                    in case z of
                         [] -> Nothing
                         x:xs -> case ys of
                                   []  -> Just $ DT.rootLabel x
                                   ys' -> findTree (DT.subForest x) ys'

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
      

--------------------------------------------------------------------------------


newtype WindowHistory = WindowHistory JSVal

navComp :: IO (Signal Path)
navComp = do
  (u, s) <- newSignal [] :: FRP (Sink Path, Signal Path)

  -- onPopstate =<< mkCallback (handleState u)
  -- void . forkIO $ getStateFromUrl >>= handleState u

  onUrlHashChange =<< mkCallback (handleLocHash u)
  void . forkIO $ getUrlHash >>=  handleLocHash' u

  pure s

  where
    -- handleState   u = u . fmap decodeURIComponent . unpackWindowHistory . toWH
    handleLocHash u = u . fmap decodeURIComponent . splitLocationHash . extractNewHash
    handleLocHash' u = u . fmap decodeURIComponent . splitLocationHash 



foreign import javascript unsafe "var f = $1; window.onpopstate = function(e) { console.log(e, f); f(e.state) };"
  onPopstate :: Callback (JSVal -> IO ()) -> IO ()    
  
unpackWindowHistory :: WindowHistory -> [JSString]
unpackWindowHistory wh = xs
  where
    xs = fmap (getWHChunk wh) [0..pred len]
    len = whLength wh

mkCallback :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
mkCallback f = syncCallback1 ThrowWouldBlock f

foreign import javascript unsafe "$1.xs.length" whLength :: WindowHistory -> Int
foreign import javascript unsafe "$1.xs[$2]" getWHChunk :: WindowHistory -> Int -> JSString
foreign import javascript unsafe "(function(z){ return z; }($1))" toWH :: JSVal -> WindowHistory

-- always leading /
foreign import javascript unsafe "(function(){ var p=location.pathname; var q=p.split('/'); var r={pushStated: true, xs: q.slice(1, q.length)}; return r; }())"
  getStateFromUrl :: IO JSVal

foreign import javascript unsafe "var x = $1; window.addEventListener('hashchange', x, false);"
  onUrlHashChange :: Callback (JSVal -> IO ()) -> IO ()

splitLocationHash :: JSString -> [JSString]
splitLocationHash h = if JSS.length h < 1 then [] else Prelude.filter (/= "") . JSS.splitOn "/" $ h

foreign import javascript unsafe "document.location.hash.replace('#', '')"
  getUrlHash :: IO JSString

foreign import javascript unsafe "(function(z){ return z.newURL.split('#')[1] || ''; }($1))"
  extractNewHash :: JSVal -> JSString 