{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Site
    ( siteComponent
    ) where

import           Data.Aeson
import qualified Data.ByteString.Char8          as BS
import qualified Data.JSString                  as JSS      
import           Data.List                      (sortOn, foldl')
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
import           Component.Notification
import           Lib
import           Net
import           Types


data ViewState = GistPending (Maybe Path) | GistError DatasourceError | GistReady Gist | Blog | AMessage JSS.JSString

data Cmd = CLock | CUnlock | CEdit EditCmd 

siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  (rootU, rootS)         <- newSignal Nothing
  navS                   <- navComponent 
  (nv, nU)               <- notificationsComponent []
  (lockU, lockS)         <- newSignal Locked
  (blogU, blogS)         <- newSignal Nothing -- :: Maybe BlogIndex
  (uiToggleU, uiToggleS) <- newSignal Site
  (lv, le)               <- loginComponent nU uiToggleU :: FRP (Signal Html, Events AuthKey)
  (ev, edU, ee)          <- editorComponent nU uiToggleU lockS :: FRP (Signal Html, Sink EditCmd, Events EditResult)
  (viewU, viewModel)     <- newSignal (GistPending Nothing)
  (stateU, stateModel)   <- newSignal [] :: FRP (Sink (DT.Forest Page), Signal (DT.Forest Page))
  (cmdU, cmdE)           <- newEvent :: FRP (Sink Cmd, Events Cmd)

  let model_ = (,) <$> stateModel <*> navS :: Signal Model_                                                 
  let model  = (,,,,) <$> stateModel <*> navS <*> lockS <*> rootS <*> blogS :: Signal Model                                                 
  let v      = view cmdU <$> viewModel <*> model
  let v'     = layout <$> uiToggleS <*> v <*> lv <*> ev <*> nv
  
  void $ titleComponent model
  void $ subscribeEvent (updates model_) $ handleModel lockS viewU
  void $ subscribeEvent cmdE $ controller uiToggleU lockU edU
  void $ subscribeEvent le $ \authkey -> uiToggleU Site >> lockU (Unlocked authkey)
  void $ subscribeEvent ee $ handleEdits lockS rootU stateU viewU

  void . forkIO $ loadMenu lockS (rootGist c) rootU stateU viewU 
  void . forkIO $ loadBlog lockS (blogGist c) blogU        viewU

  pure v'

  where 
    layout :: ViewMode -> Html -> Html -> Html -> Html -> Html
    layout s sv lv ev nv = case s of
      Site   -> H.div [] [nv, sv]
      Login  -> H.div [] [nv, wrapper' lv]
      Editor -> H.div [] [nv, overlayWrapper ev]

    handleEdits :: Signal Lock -> Sink (Maybe RootGist) -> Sink (DT.Forest Page) -> Sink ViewState -> EditResult -> FRP ()
    handleEdits lockS rootU stateU viewU (RRootGist rg) = loadMenu lockS (Types.id . digout $ rg) rootU stateU viewU 
    handleEdits lockS _     _      viewU (RGist g)      = loadGist_ lockS viewU (Types.id g) $ viewU . GistReady  -- really, navTo gist url?
    handleEdits _     _     _      viewU (RNew g)       = viewU . AMessage $ "Your new gist has been created, id = " <> getGistId (Types.id g)

    controller :: Sink ViewMode -> Sink Lock -> Sink EditCmd -> Cmd -> FRP ()
    controller uiToggleU lockU edU cmd = 
      case cmd of
        CLock           -> uiToggleU Site >> lockU Locked
        CUnlock         -> uiToggleU Login
        CEdit g         -> edU g

    handleModel :: Signal Lock -> Sink ViewState -> Model_ -> FRP ()
    handleModel lockS viewU (f, p) = 
      case findTreeByPath f p of
        Nothing -> case (f, p) of
          ([], [])           -> print "1" >> (viewU $ GistPending Nothing)
          ([], p')           -> print "2" >> (viewU . GistPending . Just $ p)
          (_, "blog":[])     -> print "3" >> (viewU . GistPending . Just $ p)
          (_, "blog":bid:[]) -> print "4" >> (loadGist_ lockS viewU (GistId bid) $ viewU . GistReady)
          (_,  p')           -> print "5" >> (viewU . GistError . NotFound $ p')
        Just page            -> case p of
                                  "blog":[] -> print "7" >> (viewU Blog)
                                  _         -> print "6" >> (loadGist_ lockS viewU (dataSource page) $ viewU . GistReady)

    loadBlog :: Signal Lock -> GistId -> Sink (Maybe BlogIndex) -> Sink ViewState -> FRP ()
    loadBlog lockS bg blogU viewU  = 
      loadGist_ lockS viewU bg $ \blogGist_ -> 
        case unfiles $ files blogGist_ of
          []    -> viewU $ GistError $ DatasourceError "There are no files in the blog forest"
          (f:_) -> let forest = eitherDecodeStrict' . BS.pack . JSS.unpack . f_content $ f :: Either String BlogIndex
                   in case forest of
                          Left err -> viewU $ GistError $ DatasourceError $ JSS.pack err
                          Right bi' -> blogU $ Just bi'
    
    loadMenu :: Signal Lock -> GistId -> Sink (Maybe RootGist) -> Sink (DT.Forest Page) -> Sink ViewState -> FRP ()
    loadMenu lockS rg rootU stateU viewU  = 
      loadGist_ lockS viewU rg $ \rootGist_ -> do
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

    loadGist_ :: Signal Lock -> Sink ViewState -> GistId -> (Gist -> IO ()) -> IO ()
    loadGist_ lockS viewU g f = void . forkIO $ do
      viewU $ GistPending Nothing
      k <- pollBehavior $ current lockS
      a <- loadGist k g :: IO (Either DatasourceError ApiResult)
      case a of
        Left x   -> viewU $ GistError x
        Right a' -> case a' of
                      Left  m   -> viewU $ GistError $ DatasourceError (message m)
                      Right a'' -> f a''

    view :: Sink Cmd -> ViewState -> Model -> Html
    view cmdU (GistPending p) m = 
      let ps = fromMaybe "" $ renderPath <$> p
      in wrapper cmdU [] [] m $ H.div [A.class_ "loader-container"] 
                                   [ H.div [] [H.text $ "Loading " <> ps]
                                   , H.img [A.class_ "ajax-loader", A.src "img/ajax-loader.gif"] [] ]

    view cmdU (GistError (DatasourceError s)) m = 
      wrapper cmdU [] [] m $ H.div [A.class_ "s500"] 
                        [ -- H.span [A.class_ "error-description"] [H.text "Error fetching data: "]
                          H.span [A.class_ "error-message"] [H.text s ]
                        -- , H.span [A.class_ "error-sorry"] [H.text " Sorry for that."]
                        ]
                   
    view cmdU (GistError (NotFound ps)) m = 
      wrapper cmdU [] [] m $ H.div [A.class_ "s404"] 
                    [ H.text "The path "
                    , H.span [A.class_ "path"] [H.text $ renderPath ps ]
                    , H.text " was not found in this forest."
                    ]
    
    view cmdU (AMessage msg) m = 
      wrapper cmdU [] [] m $ H.div [] [ H.text msg ]
    
    view cmdU Blog m@(_, _, k, _, Nothing) = 
      wrapper cmdU [] (createButton cmdU k) m $ H.div [A.class_ "loader-container"] 
                                                      [ H.div [] [H.text "Loading "]
                                                      , H.img [A.class_ "ajax-loader", A.src "img/ajax-loader.gif"] [] ]

    view cmdU Blog m@(_, _, k, _, Just bi) = 
      wrapper cmdU [] (createButton cmdU k) m (renderBlogIndex bi)

    view cmdU (GistReady g) m@(_, _, k, _, _) = 
      wrapper cmdU (editButton cmdU (Right g) k) (createButton cmdU k) m (renderGistFiles $ unfiles $ files g)
    
    wrapper :: Sink Cmd -> [Html] -> [Html] -> Model -> Html -> Html
    wrapper cmdU editH createH (f, p, k, r, _) b = 
      let menu = extractMenu f p []
      in H.div [A.class_ "content"]
               [ H.div [A.class_ "lock"] [renderLock cmdU k]
               , H.div [A.class_ "section"] $ [ renderMenu cmdU k r menu ] <> editH <> createH <> [b]]

    wrapper' :: Html -> Html
    wrapper' b = 
      H.div [A.class_ "content"]
            [H.div [A.class_ "section"] [ b ]]

    editButton :: Sink Cmd -> Either (Maybe RootGist) Gist -> Lock -> [Html]
    editButton cmdU (Right g)       (Unlocked _) = [H.button [E.click $ const $ cmdU $ CEdit $ EGist g]     [H.text "Edit"]]
    editButton cmdU (Left (Just g)) (Unlocked _) = [H.button [E.click $ const $ cmdU $ CEdit $ ERootGist g] [H.text "Edit"]]
    editButton _    (Left Nothing)  _            = [H.text "No root gist o_O"]
    editButton _    _               Locked       = []
    
    createButton :: Sink Cmd -> Lock -> [Html]
    createButton cmdU (Unlocked _) = [H.button [E.click $ const $ cmdU $ CEdit ECreate] [H.text "Create new"]]
    createButton _    _            = []
    
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

    renderGistFiles :: [File] -> Html
    renderGistFiles [] = H.div [] []
    renderGistFiles as = H.div [] (join $ fmap renderFileH as)
              
    renderFileH :: File -> [Html]
    renderFileH f = htmlStringToVirtualDom $ f_content f

    renderBlogIndex :: BlogIndex -> Html
    renderBlogIndex bidx = H.div [A.class_ "section page"] 
                                 [ H.div [A.class_ "text"] 
                                         [ H.ul [ A.class_ "articles-index" ] (join $ fmap yearsW idx') ] ]
      where
        idx  = reverse . sortOn (year) . unblog $ bidx

        idx' :: [(Int, [BlogRecord])]
        idx' = foldl' folder [] idx

        folder :: [(Int, [BlogRecord])] -> BlogRecord -> [(Int, [BlogRecord])]
        folder [] x = [(year x, [x])]
        folder a  x =
          let y        = year x
              (y', xs) = last a
          in if y == y' then init a <> [(y, xs <> [x])]
                        else a <> [(y, [x])]

        yearsW :: (Int, [BlogRecord]) -> [Html]
        yearsW (y, xs) = 
          [ H.li [ A.class_ "articles-index" ] 
                [ H.div [ A.class_ "article-index-year"] [ H.text $ showJS y] ] 
          ] <> fmap monthsW (reverse . sortOn day . reverse . sortOn month $ xs)

        monthsW :: BlogRecord -> Html
        monthsW x = 
          H.li [ A.class_ "articles-index" ]
              [ H.div [ A.class_ "article-index-title" ]
                      [ H.span [ A.class_ "article-index-title-date" ] 
                                [ H.text $ showJS (day x) <> "/" <> showJS (month x) ] 
                      , H.a [ A.href $ "#blog/" <> slug x ] [ H.text $ humanTitle x ]
                      ]
              ]
          
