{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Lib
    ( siteComponent
    ) where

import GHCJS.Types (JSString, JSVal)
import Data.String (fromString)
import Data.ByteString
import Data.Monoid ((<>))
import Data.Maybe (listToMaybe)
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


data GistStatus = GistPending | GistError DatasourceError | GistReady Menu Gist 


siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  navS <- navComp 
  (viewU, viewModel) <- newSignal GistPending
  (stateU, stateModel) <- newSignal [] :: FRP (Sink (DT.Forest Page), Signal (DT.Forest Page))

  let stateModel' = (,) <$> stateModel <*> navS :: Signal (DT.Forest Page, Path)                                                 

  subscribeEvent (updates stateModel') $ \(f, p) -> do
    let menu = extractMenu f p []

    case findTree f p of
      Nothing -> case (f, p) of
        ([], []) -> print "entering the forest" >> (viewU . GistError . DatasourceError menu $ "Entering the forest")
        (_, "blog":bid:[]) -> loadGist_ viewU (GistId bid) $ viewU . GistReady menu 
        (f, p) -> print ("path not found in the forest" <> show (f,p)) >> (viewU . GistError . NotFound menu $ p)
      Just page -> loadGist_ viewU (dataSource page) $ viewU . GistReady menu  

  let v = fmap view viewModel

  loadGist_ viewU (rootGist c) $ \a -> 
    case unfiles $ files a of
      [] -> viewU $ GistError $ DatasourceError emptyMenu "There are no files in this forest"
      (f:fs) -> do
        let forest = eitherDecodeStrict' . BS.pack . JSS.unpack . f_content $ f :: Either String (DT.Forest Page)
        case forest of
              Left err -> viewU $ GistError $ DatasourceError emptyMenu $ JSS.pack err
              Right forest' -> stateU forest'

  pure v

  where 
    extractMenu :: DT.Forest Page -> Path -> Path -> Menu
    extractMenu f [] bc = [fmap (\x -> let p = DT.rootLabel x in MIUnselected (title p) (bc <> [path p])) f]
    extractMenu f (p0:ps) bc = 
      let topMenu = fmap DT.rootLabel f
          topMenu' = fmap (\p -> if path p == p0 then MISelected (title p) (bc <> [path p]) else MIUnselected (title p) (bc <> [path p])) topMenu
          curTree = listToMaybe $ Prelude.filter ((p0 ==) . path . DT.rootLabel) f
          subMenu = case curTree of
            Nothing -> []
            Just t -> extractMenu (DT.subForest t) ps (bc <> [path $ DT.rootLabel t])
      in [topMenu', join subMenu]

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
    view (GistError (DatasourceError m s)) = 
      H.div [A.class_ "content"]
            [H.div [A.class_ "section"]
                   [ renderMenu m
                   , H.div [A.class_ "500"] 
                           [ H.span [A.class_ "error-description"] [H.text "Error fetching data:"]
                           , H.span [A.class_ "error-message"] [H.text s ]
                           , H.span [A.class_ "error-sorry"] [H.text "Sorry for that."]
                           ]
                   ]]
    view (GistError (NotFound m ps)) = 
      H.div [A.class_ "content"]
            [H.div [A.class_ "section"]
                   [ renderMenu m
                   , H.div [A.class_ "404"] 
                           [ H.text "The path "
                           , H.span [A.class_ "path"] [H.text $ renderPath ps ]
                           , H.text " was not found."
                           ]
                   ]]
    view (GistReady m a) = 
      H.div [A.class_ "content"]
            [H.div [A.class_ "section"]
                   [ renderMenu m
                   , gistH $ unfiles $ files a
                   ]]

    renderMenu :: Menu -> Html
    renderMenu [] =  H.div [A.class_ "nav"] []
    renderMenu xs = H.div [A.class_ "nav"] (go 0 xs)

    go lvl [] = []
    go lvl (m:sm) = 
      [H.div [A.class_ $ "menu-level-" <> showJS lvl] 
             (fmap menuItem m)
      ] <> (go (lvl+1) sm)

    menuItem (MISelected x ps) = H.a [A.class_ "current-menu-item", A.href (renderPath ps)] [ H.text x ]
    menuItem (MIUnselected x ps) = H.a [A.href (renderPath ps)] [ H.text x ]

    renderPath ps = "#" <> JSS.intercalate "/" ps

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