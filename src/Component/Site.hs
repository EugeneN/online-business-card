{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Site
    ( siteComponent
    ) where

import           GHCJS.Types                    (JSString)
import           Data.Aeson
import qualified Data.ByteString.Char8          as BS
import qualified Data.JSString                  as JSS      
import           Data.Monoid                    ((<>))
import qualified Data.Tree                      as DT
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void, join)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A        

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)

import           Component.Nav
import           Component.Title
import           Lib
import           Net
import           Types


data GistStatus = GistPending | GistError DatasourceError | GistReady Gist 



--------------------------------------------------------------------------------    

siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  navS <- navComponent 
  (viewU, viewModel)   <- newSignal GistPending
  (stateU, stateModel) <- newSignal [] :: FRP (Sink (DT.Forest Page), Signal (DT.Forest Page))

  let model = (,) <$> stateModel <*> navS :: Signal (DT.Forest Page, Path)                                                 
  let v     = fmap view ((,) <$> viewModel <*> model)
  
  void $ titleComponent model

  void $ subscribeEvent (updates model) $ \(f, p) -> 
    case findTreeByPath f p of
      Nothing -> case (f, p) of
        ([], [])           -> viewU . GistError . DatasourceError $ "Entering the forest"
        (_, "blog":bid:[]) -> loadGist_ viewU (GistId bid) $ viewU . GistReady
        ([], p')           -> viewU . GistError . Waiting $ p'
        (_,  p')           -> viewU . GistError . NotFound $ p'
      Just page            -> loadGist_ viewU (dataSource page) $ viewU . GistReady

  loadGist_ viewU (rootGist c) $ \a -> 
    case unfiles $ files a of
      []    -> viewU $ GistError $ DatasourceError "There are no files in this forest"
      (f:_) -> 
        let forest = eitherDecodeStrict' . BS.pack . JSS.unpack . f_content $ f :: Either String (DT.Forest Page)
        in case forest of
              Left err      -> viewU $ GistError $ DatasourceError $ JSS.pack err
              Right forest' -> stateU forest'

  pure v

  where 
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
                      Left  m   -> viewU $ GistError $ DatasourceError (message m)
                      Right a'' -> f a''

    view :: (GistStatus, (DT.Forest Page, Path)) -> Html
    view (GistPending, m) = 
      wrapper m $ H.div [A.class_ "loader-container"] 
                        [ H.img [A.class_ "ajax-loader", A.src "img/ajax-loader.gif"] []
                        , H.text "Loading" ]

    view (GistError (DatasourceError s), m) = 
      wrapper m $ H.div [A.class_ "s500"] 
                        [ -- H.span [A.class_ "error-description"] [H.text "Error fetching data: "]
                          H.span [A.class_ "error-message"] [H.text s ]
                        -- , H.span [A.class_ "error-sorry"] [H.text " Sorry for that."]
                        ]
                   
    view (GistError (Waiting ps), m) = 
      wrapper m $ H.div [A.class_ "s404"] 
                        [ H.text "Waiting for the forest to grow up at the path "
                        , H.span [A.class_ "path"] [H.text $ renderPath ps ]
                        , H.text "."
                        ]
                   
    view (GistError (NotFound ps), m) = 
      wrapper m $ H.div [A.class_ "s404"] 
                    [ H.text "The path "
                    , H.span [A.class_ "path"] [H.text $ renderPath ps ]
                    , H.text " was not found in this forest."
                    ]
      
    view (GistReady a, m) = 
      wrapper m (gistH $ unfiles $ files a)
    
    wrapper :: (DT.Forest Page, Path) -> Html -> Html
    wrapper (f, p) b = 
      let menu = extractMenu f p []
      in H.div [A.class_ "content"]
            [H.div [A.class_ "section"]
                   [ renderMenu menu, b ]]

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



--------------------------------------------------------------------------------


