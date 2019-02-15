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
import           Control.Concurrent             (forkIO, threadDelay)
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
import           Lib
import           Net
import           Types


data ViewState = GistPending (Maybe Path) | GistError DatasourceError | GistReady Gist 

data LockCmd = CLock | CUnlock deriving (Show)

siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  navS                         <- navComponent 
  (loginToggleU, loginToggleS) <- newSignal Site
  (lv, le)                     <- loginComponent loginToggleU :: FRP (Signal Html, Events AuthKey)
  (viewU, viewModel)           <- newSignal (GistPending Nothing)
  (stateU, stateModel)         <- newSignal [] :: FRP (Sink (DT.Forest Page), Signal (DT.Forest Page))
  (lockCmdU, lockCmdE)         <- newEvent :: FRP (Sink LockCmd, Events LockCmd)
  (lockU, lockS)               <- newSignal Locked

  let model = (,,) <$> stateModel <*> navS <*> lockS :: Signal Model                                                 
  let v     = view lockCmdU <$> viewModel <*> model
  let v'    = layout <$> loginToggleS <*> v <*> (fmap wrapper' lv)
  
  void $ titleComponent model
  void $ subscribeEvent (updates model) $ handleModel viewU
  void $ subscribeEvent lockCmdE $ controller loginToggleU lockU
  void $ subscribeEvent le $ \authkey -> do
    loginToggleU Site 
    threadDelay 100000 -- TODO prevent overlapping renderings in runAppReactive
    lockU (Unlocked authkey)

  loadMenu stateU viewU

  pure v'

  where 
    layout :: ViewMode -> Html -> Html -> Html
    layout s sv lv = case s of
      Site  -> sv
      Login -> lv

    controller :: Sink ViewMode -> Sink Lock -> LockCmd -> FRP ()
    controller loginToggleU lockU cmd = do
      print cmd
      case cmd of
        CLock      -> loginToggleU Site >> lockU Locked
        CUnlock    -> loginToggleU Login

    handleModel :: Sink ViewState -> Model -> FRP ()
    handleModel viewU (f, p, _) = 
      case findTreeByPath f p of
        Nothing -> case (f, p) of
          ([], [])           -> viewU (GistPending Nothing)
          (_, "blog":bid:[]) -> loadGist_ viewU (GistId bid) $ viewU . GistReady
          ([], p')           -> viewU . GistPending . Just $ p'
          (_,  p')           -> viewU . GistError . NotFound $ p'
        Just page            -> loadGist_ viewU (dataSource page) $ viewU . GistReady

    loadMenu :: Sink (DT.Forest Page) -> Sink ViewState -> FRP ()
    loadMenu stateU viewU = 
      loadGist_ viewU (rootGist c) $ \a -> 
        case unfiles $ files a of
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

    view :: Sink LockCmd -> ViewState -> Model -> Html
    view lockCmdU (GistPending p) m = 
      let ps = fromMaybe "" $ renderPath <$> p
      in wrapper lockCmdU m $ H.div [A.class_ "loader-container"] 
                           [ H.div [] [H.text $ "Loading " <> ps]
                           , H.img [A.class_ "ajax-loader", A.src "img/ajax-loader.gif"] [] ]

    view lockCmdU (GistError (DatasourceError s)) m = 
      wrapper lockCmdU m $ H.div [A.class_ "s500"] 
                        [ -- H.span [A.class_ "error-description"] [H.text "Error fetching data: "]
                          H.span [A.class_ "error-message"] [H.text s ]
                        -- , H.span [A.class_ "error-sorry"] [H.text " Sorry for that."]
                        ]
                   
    view lockCmdU (GistError (NotFound ps)) m = 
      wrapper lockCmdU m $ H.div [A.class_ "s404"] 
                    [ H.text "The path "
                    , H.span [A.class_ "path"] [H.text $ renderPath ps ]
                    , H.text " was not found in this forest."
                    ]
    
    view lockCmdU (GistReady a) m = 
      wrapper lockCmdU m (gistH $ unfiles $ files a)
    
    wrapper :: Sink LockCmd -> Model -> Html -> Html
    wrapper lockCmdU (f, p, k) b = 
      let menu = extractMenu f p []
      in H.div [A.class_ "content"]
            [ H.div [A.class_ "lock"] [renderLock lockCmdU k]
            , H.div [A.class_ "section"]
                   [ renderMenu menu, b ]]

    wrapper' :: Html -> Html
    wrapper' b = 
      H.div [A.class_ "content"]
            [H.div [A.class_ "section"] [ b ]]

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

    renderLock :: Sink LockCmd -> Lock -> Html
    renderLock lockCmdU Locked       = H.button [A.class_ "locked",  E.click $ const $ lockCmdU CUnlock] [ H.img [A.src "https://image.flaticon.com/icons/svg/121/121685.svg"] [] ]
    renderLock lockCmdU (Unlocked _) = H.button [A.class_ "unlocked", E.click $ const $ lockCmdU CLock]  [ H.img [A.src "https://image.flaticon.com/icons/svg/121/121684.svg"] [] ]

    renderPath :: Path -> JSString
    renderPath ps = "#" <> JSS.intercalate "/" ps

    gistH :: [File] -> Html
    gistH as = H.div [] (join $ fmap renderFileH as)
              
    renderFileH :: File -> [Html]
    renderFileH f = htmlStringToVirtualDom $ f_content f
      
