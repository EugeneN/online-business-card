{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Lib
    ( siteComponent
    ) where

import           GHCJS.Types                    (JSString, JSVal)
import           GHCJS.Foreign.Callback
import           Data.Aeson
import qualified Data.ByteString.Char8          as BS
import qualified Data.JSString                  as JSS      
import           Data.Maybe                     (listToMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Tree                      as DT
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void, join)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A        

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)
import           Lubeck.Web.URI                 (decodeURIComponent)

import           Net
import           Types
import           Utils


data GistStatus = GistPending | GistError DatasourceError | GistReady Menu Gist 

siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  navS <- navComp 
  tU   <- titleComponent
  (viewU, viewModel)   <- newSignal GistPending
  (stateU, stateModel) <- newSignal [] :: FRP (Sink (DT.Forest Page), Signal (DT.Forest Page))

  let model = (,) <$> stateModel <*> navS :: Signal (DT.Forest Page, Path)                                                 
  let v     = fmap view viewModel
  
  void $ subscribeEvent (updates model) $ \(f, p) -> 
    let menu = extractMenu f p []
    in case findTreeByPath f p of
          Nothing -> case (f, p) of
            ([], [])           -> tU menu >> (viewU . GistError $ DatasourceError menu "Entering the forest")
            (_, "blog":bid:[]) -> tU menu >> (loadGist_ viewU (GistId bid) $ viewU . GistReady menu)
            ([], p')           -> tU menu >> (viewU . GistError . Waiting menu $ p')
            (_,  p')           -> tU menu >> (viewU . GistError . NotFound menu $ p')
          Just page            -> tU menu >> (loadGist_ viewU (dataSource page) $ viewU . GistReady menu)

  loadGist_ viewU (rootGist c) $ \a -> 
    case unfiles $ files a of
      []    -> viewU $ GistError $ DatasourceError emptyMenu "There are no files in this forest"
      (f:_) -> 
        let forest = eitherDecodeStrict' . BS.pack . JSS.unpack . f_content $ f :: Either String (DT.Forest Page)
        in case forest of
              Left err      -> viewU $ GistError $ DatasourceError emptyMenu $ JSS.pack err
              Right forest' -> stateU forest'

  pure v

  where 
    treeToMenuItem :: Path -> DT.Tree Page -> MenuItem
    treeToMenuItem bc t = let p = DT.rootLabel t in MIUnselected (title p) (bc <> [path p])
    
    pageToMenuItem :: Url -> Path -> Page -> MenuItem
    pageToMenuItem p0 bc p = 
      if path p == p0 
        then MISelected   (title p) (bc <> [path p]) 
        else MIUnselected (title p) (bc <> [path p])
    
    extractMenu :: DT.Forest Page -> Path -> Path -> Menu
    extractMenu f []      bc = Menu (MenuLevel $ fmap (treeToMenuItem bc) f) MenuNil
    extractMenu f (p0:ps) bc = 
      let curLevel = MenuLevel . fmap (pageToMenuItem p0 bc) . fmap DT.rootLabel $ f
          curTree  = listToMaybe $ Prelude.filter ((p0 ==) . path . DT.rootLabel) f
          subLevel = case curTree of
                        Nothing -> MenuNil
                        Just t  -> extractMenu (DT.subForest t) ps (bc <> [path $ DT.rootLabel t])
      in Menu curLevel subLevel

    findTreeByPath :: DT.Forest Page -> Path -> Maybe Page
    findTreeByPath f p = case (f, p) of
      ([],_)     -> Nothing
      (x:_, [])  -> Just $ DT.rootLabel x
      (xs, y:ys) -> case Prelude.filter ((y ==) . path . DT.rootLabel) xs of
                      []  -> Nothing
                      x:_ -> case ys of
                                []  -> Just $ DT.rootLabel x
                                ys' -> findTreeByPath (DT.subForest x) ys'

    loadGist_ :: Sink GistStatus -> GistId -> (Gist -> IO ()) -> IO ()
    loadGist_ viewU g f = void . forkIO $ do
      a <- loadGist g :: IO (Either DatasourceError ApiResult)
      case a of
        Left x   -> viewU $ GistError x
        Right a' -> case a' of
                      Left  m   -> viewU $ GistError $ DatasourceError emptyMenu (message m)
                      Right a'' -> f a''

    view :: GistStatus -> Html
    view GistPending = H.div [A.class_ "loader-container"] 
                             [ H.img [A.class_ "ajax-loader", A.src "img/ajax-loader.gif"] []
                             , H.text "Loading" ]

    view (GistError (DatasourceError m s)) = 
      wrapper m $ H.div [A.class_ "s500"] 
                        [ -- H.span [A.class_ "error-description"] [H.text "Error fetching data: "]
                          H.span [A.class_ "error-message"] [H.text s ]
                        -- , H.span [A.class_ "error-sorry"] [H.text " Sorry for that."]
                        ]
                   
    view (GistError (Waiting m ps)) = 
      wrapper m $ H.div [A.class_ "s404"] 
                        [ H.text "Waiting for the forest to grow up at the path "
                        , H.span [A.class_ "path"] [H.text $ renderPath ps ]
                        , H.text "."
                        ]
                   
    view (GistError (NotFound m ps)) = 
      wrapper m $ H.div [A.class_ "s404"] 
                    [ H.text "The path "
                    , H.span [A.class_ "path"] [H.text $ renderPath ps ]
                    , H.text " was not found in this forest."
                    ]
      
    view (GistReady m a) = 
      wrapper m (gistH $ unfiles $ files a)
    
    wrapper :: Menu -> Html -> Html
    wrapper m b = 
      H.div [A.class_ "content"]
            [H.div [A.class_ "section"]
                   [ renderMenu m, b ]]

    renderMenu :: Menu -> Html
    renderMenu m = H.div [A.class_ "nav"] (renderSubMenu 0 m)

    renderSubMenu :: Int -> Menu -> [Html]
    renderSubMenu _ MenuNil                       = []
    renderSubMenu _ (Menu (MenuLevel []) _)       = []
    renderSubMenu l (Menu (MenuLevel xs) MenuNil) = renderMenuLevel l xs
    renderSubMenu l (Menu (MenuLevel xs) sm)      = renderMenuLevel l xs <> renderSubMenu (l+1) sm

    renderMenuLevel :: Int -> [MenuItem] -> [Html]
    renderMenuLevel _   [] = []
    renderMenuLevel lvl m  = [H.div [A.class_ $ "menu-level-" <> showJS lvl] (fmap renderMenuItem m)]

    renderMenuItem :: MenuItem -> Html
    renderMenuItem (MISelected x ps)   = H.a [A.class_ "current-menu-item", A.href (renderPath ps)] [ H.text x ]
    renderMenuItem (MIUnselected x ps) = H.a [A.href (renderPath ps)] [ H.text x ]

    renderPath :: Path -> JSString
    renderPath ps = "#" <> JSS.intercalate "/" ps

    gistH :: [File] -> Html
    gistH as = H.div [] (join $ fmap renderFileH as)
              
    renderFileH :: File -> [Html]
    renderFileH f = htmlStringToVirtualDom $ f_content f
      

--------------------------------------------------------------------------------

titleComponent :: FRP (Sink Menu)
titleComponent = do
  (u, s) <- newSignal emptyMenu
  let s' = fmap (JSS.intercalate " ← " . reverse . ("EN" :) . fmap getTitle . flattenMenu) s

  void $ subscribeEvent (updates s') setTitle

  pure u

  where
    flattenMenu MenuNil          = []
    flattenMenu (Menu m MenuNil) = findSelectedItem m
    flattenMenu (Menu m sm)      = findSelectedItem m <> flattenMenu sm

    findSelectedItem (MenuLevel xs) = Prelude.filter isSelected xs

    isSelected (MISelected   _ _) = True
    isSelected (MIUnselected _ _) = False
    
    getTitle (MISelected   x _) = x
    getTitle (MIUnselected x _) = x

foreign import javascript unsafe "document.title = $1"
  setTitle :: JSString -> IO ()

--------------------------------------------------------------------------------


navComp :: IO (Signal Path)
navComp = do
  (u, s) <- newSignal [] :: FRP (Sink Path, Signal Path)

  onUrlHashChange =<< mkCallback (handleLocHash u)
  void . forkIO $ getUrlHash >>= handleLocHash' u

  pure s

  where
    handleLocHash u  = u . fmap decodeURIComponent . splitLocationHash . extractNewHash
    handleLocHash' u = u . fmap decodeURIComponent . splitLocationHash 

mkCallback :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
mkCallback f = syncCallback1 ThrowWouldBlock f

foreign import javascript unsafe "var x = $1; window.addEventListener('hashchange', x, false);"
  onUrlHashChange :: Callback (JSVal -> IO ()) -> IO ()

splitLocationHash :: JSString -> [JSString]
splitLocationHash h = if JSS.length h < 1 then [] else Prelude.filter (/= "") . JSS.splitOn "/" $ h

foreign import javascript unsafe "document.location.hash.replace('#', '')"
  getUrlHash :: IO JSString

foreign import javascript unsafe "(function(z){ return z.newURL.split('#')[1] || ''; }($1))"
  extractNewHash :: JSVal -> JSString 