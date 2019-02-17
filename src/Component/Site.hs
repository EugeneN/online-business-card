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
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Tree                      as DT
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void, join)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A    
import qualified Web.VirtualDom.Html.Events     as E      

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)

import           Component.Nav
import           Component.Title
import           Component.Login
import           Component.Editor
import           Lib
import           Net
import           Types


data ViewState = GistPending (Maybe Path) | GistError DatasourceError | GistReady Gist 

data Cmd = CLock | CUnlock | CEdit (Either RootGist Gist) deriving (Show)

siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  (rootU, rootS)         <- newSignal Nothing
  navS                   <- navComponent 
  (lockU, lockS)         <- newSignal Locked
  (uiToggleU, uiToggleS) <- newSignal Site
  (lv, le)               <- loginComponent uiToggleU :: FRP (Signal Html, Events AuthKey)
  (ev, edU, ee)          <- editorComponent uiToggleU lockS :: FRP (Signal Html, Sink (Either RootGist Gist), Events (Either RootGist Gist))
  (viewU, viewModel)     <- newSignal (GistPending Nothing)
  (stateU, stateModel)   <- newSignal [] :: FRP (Sink (DT.Forest Page), Signal (DT.Forest Page))
  (cmdU, cmdE)           <- newEvent :: FRP (Sink Cmd, Events Cmd)

  let model_ = (,) <$> stateModel <*> navS :: Signal Model_                                                 
  let model  = (,,,) <$> stateModel <*> navS <*> lockS <*> rootS :: Signal Model                                                 
  let v      = view cmdU <$> viewModel <*> model
  let v'     = layout <$> uiToggleS <*> v <*> lv <*> ev
  
  void $ titleComponent model
  void $ subscribeEvent (updates model_) $ handleModel viewU
  void $ subscribeEvent cmdE $ controller uiToggleU lockU edU
  void $ subscribeEvent le $ \authkey -> uiToggleU Site >> lockU (Unlocked authkey)
  void $ subscribeEvent ee $ handleEdits rootU stateU viewU

  loadMenu (rootGist c) rootU stateU viewU 

  pure v'

  where 
    layout :: ViewMode -> Html -> Html -> Html -> Html
    layout s sv lv ev = case s of
      Site   -> H.div [] [sv]
      Login  -> H.div [] [wrapper' lv]
      Editor -> H.div [] [overlayWrapper ev]

    handleEdits :: Sink (Maybe RootGist) -> Sink (DT.Forest Page) -> Sink ViewState -> Either RootGist Gist -> FRP ()
    handleEdits rootU stateU viewU (Left rg) = loadMenu (Types.id . digout $ rg) rootU stateU viewU 
    handleEdits _     _      viewU (Right g) = loadGist_ viewU (Types.id g) $ viewU . GistReady  -- really, navTo gist url?

    controller :: Sink ViewMode -> Sink Lock -> Sink (Either RootGist Gist) -> Cmd -> FRP ()
    controller uiToggleU lockU edU cmd = 
      case cmd of
        CLock           -> uiToggleU Site >> lockU Locked
        CUnlock         -> uiToggleU Login
        CEdit g         -> edU g

    handleModel :: Sink ViewState -> Model_ -> FRP ()
    handleModel viewU (f, p) = 
      case findTreeByPath f p of
        Nothing -> case (f, p) of
          ([], [])           -> viewU (GistPending Nothing)
          (_, "blog":bid:[]) -> loadGist_ viewU (GistId bid) $ viewU . GistReady
          ([], p')           -> viewU . GistPending . Just $ p'
          (_,  p')           -> viewU . GistError . NotFound $ p'
        Just page            -> loadGist_ viewU (dataSource page) $ viewU . GistReady

    loadMenu :: GistId -> Sink (Maybe RootGist) -> Sink (DT.Forest Page) -> Sink ViewState -> FRP ()
    loadMenu rg rootU stateU viewU  = 
      loadGist_ viewU rg $ \rootGist_ -> do
        rootU $ Just $ RootGist rootGist_
        case unfiles $ files rootGist_ of
          []    -> viewU $ GistError $ DatasourceError "There are no files in this forest"
          (f:_) -> let forest = eitherDecodeStrict' . BS.pack . JSS.unpack . f_content $ f :: Either String (DT.Forest Page)
                   in case forest of
                          Left err      -> viewU $ GistError $ DatasourceError $ JSS.pack err
                          Right forest' -> stateU forest'

    findTreeByPath :: DT.Forest Page -> Path -> Maybe Page
    findTreeByPath f p = case (f, p) of
      ([],_)     -> Nothing
      (x:_, [])  -> Just $ DT.rootLabel x
      (xs, y:ys) -> case Prelude.filter ((y ==) . path . DT.rootLabel) xs of
                      []  -> Nothing
                      x:_ -> case ys of
                                []  -> Just $ DT.rootLabel x
                                ys' -> findTreeByPath (DT.subForest x) ys'

    loadGist_ :: Sink ViewState -> GistId -> (Gist -> IO ()) -> IO ()
    loadGist_ viewU g f = void . forkIO $ do
      viewU $ GistPending Nothing
      a <- loadGist g :: IO (Either DatasourceError ApiResult)
      case a of
        Left x   -> viewU $ GistError x
        Right a' -> case a' of
                      Left  m   -> viewU $ GistError $ DatasourceError (message m)
                      Right a'' -> f a''

    view :: Sink Cmd -> ViewState -> Model -> Html
    view cmdU (GistPending p) m = 
      let ps = fromMaybe "" $ renderPath <$> p
      in wrapper cmdU [] m $ H.div [A.class_ "loader-container"] 
                           [ H.div [] [H.text $ "Loading " <> ps]
                           , H.img [A.class_ "ajax-loader", A.src "img/ajax-loader.gif"] [] ]

    view cmdU (GistError (DatasourceError s)) m = 
      wrapper cmdU [] m $ H.div [A.class_ "s500"] 
                        [ -- H.span [A.class_ "error-description"] [H.text "Error fetching data: "]
                          H.span [A.class_ "error-message"] [H.text s ]
                        -- , H.span [A.class_ "error-sorry"] [H.text " Sorry for that."]
                        ]
                   
    view cmdU (GistError (NotFound ps)) m = 
      wrapper cmdU [] m $ H.div [A.class_ "s404"] 
                    [ H.text "The path "
                    , H.span [A.class_ "path"] [H.text $ renderPath ps ]
                    , H.text " was not found in this forest."
                    ]
    
    view cmdU (GistReady g) m@(_, _, k, _) = 
      wrapper cmdU (editButton cmdU (Right g) k) m (gistH $ unfiles $ files g)
    
    wrapper :: Sink Cmd -> [Html] -> Model -> Html -> Html
    wrapper cmdU editH (f, p, k, r) b = 
      let menu = extractMenu f p []
      in H.div [A.class_ "content"]
               [ H.div [A.class_ "lock"] [renderLock cmdU k]
               , H.div [A.class_ "section"] $ [ renderMenu cmdU k r menu ] <> editH <> [b]]


    wrapper' :: Html -> Html
    wrapper' b = 
      H.div [A.class_ "content"]
            [H.div [A.class_ "section"] [ b ]]

    editButton :: Sink Cmd -> Either (Maybe RootGist) Gist -> Lock -> [Html]
    editButton cmdU (Right g)       (Unlocked _) = [H.button [E.click $ const $ cmdU $ CEdit $ Right g] [H.text "Edit"]]
    editButton cmdU (Left (Just g)) (Unlocked _) = [H.button [E.click $ const $ cmdU $ CEdit $ Left g]  [H.text "Edit"]]
    editButton _    (Left Nothing)  _            = [H.text "No root gist o_O"]
    editButton _    _               Locked       = []
    
    overlayWrapper :: Html -> Html
    overlayWrapper b = 
      H.div [A.class_ "content overlay"]
            [H.div [A.class_ "section"] [ b ]]

    renderMenu :: Sink Cmd -> Lock -> Maybe RootGist -> Menu -> Html
    renderMenu _    Locked         _ m = H.div [A.class_ "nav"] (renderSubMenu 0 m)
    renderMenu cmdU k@(Unlocked _) r m = H.div [A.class_ "nav"] (renderSubMenu 0 m <> editButton cmdU (Left r) k)

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

    renderLock :: Sink Cmd -> Lock -> Html
    renderLock cmdU Locked       = H.button [A.class_ "locked",   E.click $ const $ cmdU CUnlock] [ H.img [A.src "https://image.flaticon.com/icons/svg/121/121685.svg"] [] ]
    renderLock cmdU (Unlocked _) = H.button [A.class_ "unlocked", E.click $ const $ cmdU CLock]   [ H.img [A.src "https://image.flaticon.com/icons/svg/121/121684.svg"] [] ]

    renderPath :: Path -> JSString
    renderPath ps = "#" <> JSS.intercalate "/" ps

    gistH :: [File] -> Html
    gistH []    = H.div [] []
    gistH as = H.div [] (join $ fmap renderFileH as)
              
    renderFileH :: File -> [Html]
    renderFileH f = htmlStringToVirtualDom $ f_content f
      
