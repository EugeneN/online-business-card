{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Site
    ( siteComponent
    ) where

import           Data.Aeson
import qualified Data.JSString                  as JSS      
import           Data.List                      (sortOn, foldl')
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.Monoid                    ((<>))
import           Data.Time.Calendar             (fromGregorian)
import qualified Data.Tree                      as DT
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Control.Applicative            ((<|>))
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


data ContentState = GistPending (Maybe Path) | GistError DatasourceError | GistReady Gist 
                  | Blog BlogIndex BlogGist | AMessage JSS.JSString

defContent :: ContentState            
defContent = GistPending Nothing

data Cmd = CLock | CUnlock | CEdit EditCmd 


siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  (cmdU, cmdE)             <- newEvent             :: FRP (Sink Cmd,                   Events Cmd)
  (rootU, rootS)           <- newSignal Nothing    :: FRP (Sink (Maybe RootGist),      Signal (Maybe RootGist))
  (bIndexU, bIndexS)       <- newSignal Nothing    :: FRP (Sink (Maybe BlogIndexFull), Signal (Maybe BlogIndexFull)) 
  (lockU, lockS)           <- newSignal Locked     :: FRP (Sink Lock,                  Signal Lock) 
  (viewModeU, viewModeS)   <- newSignal Site       :: FRP (Sink ViewMode,              Signal ViewMode) 
  (contentU, contentModel) <- newSignal defContent :: FRP (Sink ContentState,          Signal ContentState) 
  (structU, structS)       <- newSignal emptyArea  :: FRP (Sink Area,                  Signal Area)
  navS                     <- navComponent         :: IO  (Signal Path)
  (nv, nU)                 <- notificationsComponent []          :: FRP (Signal Html, Sink (Maybe Notification))
  (lv, le)                 <- loginComponent  nU viewModeU       :: FRP (Signal Html, Events AuthKey)
  (ev, edU, ee)            <- editorComponent nU viewModeU lockS :: FRP (Signal Html, Sink EditCmd, Events EditResult)

  let model  = (,,) <$> structS <*> navS <*> bIndexS :: Signal Model
  let model_ = (,)  <$> structS <*> navS             :: Signal Model_                                                 

  let menuV  = renderMenu        cmdU <$> model_
  let menuTV = renderMenuToolbar cmdU <$> lockS <*> rootS
  let lockV  = renderLock        cmdU <$> lockS
  let v      = view              cmdU <$> lockS <*> contentModel <*> structS
  let v'     = layout <$> viewModeS <*> v <*> lv <*> ev <*> nv <*> menuV <*> lockV <*> menuTV
  
  void $ titleComponent                    model 
  void $ subscribeEvent (updates navS)     handleRedirects
  void $ subscribeEvent (updates model)  $ handleBlogPage lockS contentU
  void $ subscribeEvent (updates model_) $ handleTreePage lockS contentU
  void $ subscribeEvent ee               $ handleEdits    lockS contentU rootU bIndexU structU
  void $ subscribeEvent cmdE             $ handleCmd      viewModeU lockU edU
  void $ subscribeEvent le               $ handleLogin    viewModeU lockU

  void . forkIO $ loadStruct    lockS (rootGist c) rootU   structU contentU 
  void . forkIO $ loadBlogIndex lockS (blogGist c) bIndexU         contentU

  pure v'

  where 
    layout :: ViewMode -> Html -> Html -> Html -> Html -> Html -> Html -> [Html] -> Html
    layout s bv lv ev nv mv kv mtv = case s of
      Site   -> H.div [] [nv, siteWrapper mv mtv kv bv]
      Login  -> H.div [] [nv, loginWrapper lv]
      Editor -> H.div [] [nv, overlayWrapper ev]

    handleRedirects :: Path -> IO ()
    handleRedirects p = case redirects p of
      Just p' -> redirectLocal p'
      Nothing -> pure ()

    handleLogin :: Sink ViewMode -> Sink Lock -> AuthKey -> FRP ()
    handleLogin viewModeU lockU authkey = viewModeU Site >> lockU (Unlocked authkey)

    handleEdits :: Signal Lock -> Sink ContentState -> Sink (Maybe RootGist) -> Sink (Maybe BlogIndexFull) -> Sink Area -> EditResult -> FRP ()
    handleEdits lockS contentU rootU _       structU (RRootGist rg) = loadStruct    lockS (Types.id . digout $ rg)  rootU  structU contentU 
    handleEdits lockS contentU _     bIndexU _       (RBlogGist bg) = loadBlogIndex lockS (Types.id . blogout $ bg) bIndexU        contentU 
    handleEdits lockS contentU _     _       _       (RGist g)      = loadGist_     lockS (Types.id g) contentU $ contentU . GistReady                                -- really, navTo gist url?
    handleEdits _     contentU _     _       _       (RNew g)       = contentU . AMessage $ "Your new gist has been created, id = " <> getGistId (Types.id g)

    handleCmd :: Sink ViewMode -> Sink Lock -> Sink EditCmd -> Cmd -> FRP ()
    handleCmd viewModeU lockU edU cmd = 
      case cmd of
        CLock   -> viewModeU Site >> lockU Locked
        CUnlock -> viewModeU Login
        CEdit g -> edU g

    handleBlogPage :: Signal Lock -> Sink ContentState -> Model -> FRP ()
    handleBlogPage lockS contentU (area, p:ps, bi) | isBlog (blogSlug area) (p:ps) = case (bi, ps) of
      (Nothing,         [])     -> contentU . GistPending . Just $ p:ps -- blog index
      (Nothing,         _:[])   -> contentU . GistPending . Just $ p:ps -- blog article
      (Just (bi', big), [])     -> contentU $ Blog bi' big
      (Just (bi', _),   bid:[]) -> loadBlog lockS contentU bi' bid
      _                         -> pure ()
    
    handleBlogPage _ _ _ = pure ()

    handleTreePage :: Signal Lock -> Sink ContentState -> Model_ -> FRP ()
    handleTreePage _     _     ((Area bs _ _ _ _ _), p) | isBlog bs p = pure ()
    handleTreePage lockS contentU ((Area _  _ _ _ f hf), p)            = case (f, p) of
      ([], [])  -> contentU $ GistPending Nothing
      ([], p')  -> contentU . GistPending . Just $ p'
      (m:_, []) -> loadGist_ lockS (dataSource . DT.rootLabel $ m) contentU $ contentU . GistReady -- home page
      (ms, ps)  -> case findTreeByPath ms ps of
                     Nothing   -> case findTreeByPath hf p of
                                    Nothing         -> contentU . GistError . NotFound $ ps
                                    Just hiddenPage -> loadGist_ lockS (dataSource hiddenPage) contentU $ contentU . GistReady
                     Just page -> loadGist_ lockS (dataSource page) contentU $ contentU . GistReady
    
    loadBlog :: Signal Lock -> Sink ContentState -> BlogIndex -> Url -> FRP ()
    loadBlog lockS contentU (BlogIndex bi) s = do
      contentU . GistPending . Just $ [s]
      case listToMaybe (Prelude.filter ((s ==) . slug) bi) <|> 
           listToMaybe (Prelude.filter ((s ==) . hash) bi) of
        Nothing -> contentU . GistError . NotFound $ [s]
        Just x  -> loadGist_ lockS (GistId $ hash x) contentU $ contentU . GistReady
    
    loadBlogIndex :: Signal Lock -> GistId -> Sink (Maybe BlogIndexFull) -> Sink ContentState -> FRP ()
    loadBlogIndex lockS bg bIndexU contentU  = 
      loadGist_ lockS bg contentU $ \blogGist_ -> 
        case unfiles $ files blogGist_ of
          []    -> contentU $ GistError $ DatasourceError "There are no files in this forest"
          (f:_) -> let x = eitherDecodeStrict' . TE.encodeUtf8 . T.pack . JSS.unpack . f_content $ f :: Either String BlogIndex
                   in case x of
                          Left err  -> contentU $ GistError $ DatasourceError $ JSS.pack err
                          Right bi' -> bIndexU $ Just (bi', BlogGist blogGist_)
    
    loadStruct :: Signal Lock -> GistId -> Sink (Maybe RootGist) 
               -> Sink Area -> Sink ContentState -> FRP ()
    loadStruct lockS rg rootU structU contentU  = 
      loadGist_ lockS rg contentU $ \rootGist_ -> do
        rootU $ Just $ RootGist rootGist_
        case unfiles $ files rootGist_ of
          []    -> contentU $ GistError $ DatasourceError "There are no files in this forest"
          (f:_) -> let area = eitherDecodeStrict' . TE.encodeUtf8 . T.pack . JSS.unpack . f_content $ f :: Either String Area
                   in case area of
                          Left err    -> contentU $ GistError $ DatasourceError $ JSS.pack err
                          Right area' -> structU area'

    findTreeByPath :: DT.Forest Page -> Path -> Maybe Page
    findTreeByPath f p = case (f, p) of
      ([],_)     -> Nothing
      (x:_, [])  -> Just $ DT.rootLabel x
      (xs, y:ys) -> case Prelude.filter ((y ==) . path . DT.rootLabel) xs of
                      []  -> Nothing
                      x:_ -> case ys of
                                []  -> Just $ DT.rootLabel x
                                ys' -> findTreeByPath (DT.subForest x) ys'

    loadGist_ :: Signal Lock -> GistId -> Sink ContentState -> (Gist -> IO ()) -> IO ()
    loadGist_ lockS g contentU f = void . forkIO $ do
      contentU $ GistPending Nothing
      k <- pollBehavior $ current lockS
      a <- loadGist k g :: IO (Either DatasourceError ApiResult)
      case a of
        Left x   -> contentU $ GistError x
        Right a' -> case a' of
                      Left  m   -> contentU $ GistError $ DatasourceError (message m)
                      Right a'' -> f a''

    view :: Sink Cmd -> Lock -> ContentState -> Area -> Html
    view _ _ (GistPending p) _ = 
      let ps = fromMaybe "" $ renderPath <$> p
      in H.div [A.class_ "loader-container"] 
               [ H.div [] [H.text $ "Loading " <> ps]
               , H.img [A.class_ "ajax-loader", A.src "img/ajax-loader.gif"] [] ]

    view _ _ (GistError (DatasourceError s)) _ = 
      H.div [A.class_ "s500"] 
            [ -- H.span [A.class_ "error-description"] [H.text "Error fetching data: "]
              H.span [A.class_ "error-message"] [H.text s ]
            -- , H.span [A.class_ "error-sorry"] [H.text " Sorry for that."]
            ]
                   
    view _ _ (GistError (NotFound ps)) _ = 
      H.div [A.class_ "s404"] 
            [ H.text "The path "
            , H.span [A.class_ "path"] [H.text $ renderPath ps ]
            , H.text " was not found in this forest."
            ]
    
    view _ _ (AMessage msg) _ = H.div [] [ H.text msg ]
    
    view cmdU k (Blog bi big) area = 
      H.div [] (editBlogButton cmdU big k <> createButton cmdU k <> [renderBlogIndex area bi])

    view cmdU k (GistReady g) _ = 
      H.div [] (editButton cmdU (Right g) k <> createButton cmdU k <> [renderGistFiles . unfiles . files $ g])
    
    siteWrapper :: Html -> [Html] -> Html -> Html -> Html
    siteWrapper menuH menuTH lockH bodyH = 
      H.div [A.class_ "content"]
            [ H.div [A.class_ "lock"] [lockH]
            , H.div [A.class_ "section"] $ menuTH <> [menuH, bodyH]]

    loginWrapper :: Html -> Html
    loginWrapper b = 
      H.div [A.class_ "content"]
            [H.div [A.class_ "section"] [ b ]]

    overlayWrapper :: Html -> Html
    overlayWrapper b = 
      H.div [A.class_ "content overlay"]
            [H.div [A.class_ "section"] [ b ]]

    editBlogButton :: Sink Cmd -> BlogGist -> Lock -> [Html]
    editBlogButton cmdU big (Unlocked _) = [H.button [E.click $ const $ cmdU $ CEdit $ EBlogGist big] [H.text "Edit"]]
    editBlogButton _    _   Locked       = []

    editButton :: Sink Cmd -> Either (Maybe RootGist) Gist -> Lock -> [Html]
    editButton cmdU (Right g)       (Unlocked _) = [H.button [E.click $ const $ cmdU $ CEdit $ EGist g]     [H.text "Edit"]]
    editButton cmdU (Left (Just g)) (Unlocked _) = [H.button [E.click $ const $ cmdU $ CEdit $ ERootGist g] [H.text "Edit"]]
    editButton _    (Left Nothing)  _            = []
    editButton _    _               Locked       = []
    
    createButton :: Sink Cmd -> Lock -> [Html]
    createButton cmdU (Unlocked _) = [H.button [E.click $ const $ cmdU $ CEdit ECreate] [H.text "Create new"]]
    createButton _    _            = []
    
    renderMenu :: Sink Cmd -> Model_ -> Html
    renderMenu _ ((Area _ cms _ _ f _), p) = 
      let m = extractMenu f p [] 
      in H.div [A.class_ "nav"] (renderSubMenu cms p 0 m)

    renderMenuToolbar :: Sink Cmd -> Lock -> Maybe RootGist -> [Html]
    renderMenuToolbar cmdU k r = case k of
      Locked     -> []
      Unlocked _ -> editButton cmdU (Left r) k

    renderSubMenu :: [Url] -> Path -> Int -> Menu -> [Html]
    renderSubMenu _   _ _ MenuNil                       = []
    renderSubMenu _   _ _ (Menu (MenuLevel []) _)       = []
    renderSubMenu cms p l (Menu (MenuLevel xs) MenuNil) = renderMenuLevel cms p l xs
    renderSubMenu cms p l (Menu (MenuLevel xs) sm)      = renderMenuLevel cms p l xs <> renderSubMenu cms p (l+1) sm

    renderMenuLevel :: [Url] -> Path -> Int -> [MenuItem] -> [Html]
    renderMenuLevel _   _ _   []                      = []
    renderMenuLevel cms z 0   m | isSpecialPath z cms = [H.div [A.class_ $ "menu-level menu-level-special menu-level-" <> showJS 0]   (fmap renderMenuItem m)]
    renderMenuLevel _   _ lvl m                       = [H.div [A.class_ $ "menu-level                    menu-level-" <> showJS lvl] (fmap renderMenuItem m)]

    renderMenuItem :: MenuItem -> Html
    renderMenuItem (MISelected   x ps True)  = H.a [A.class_ "current-menu-item-special", A.href (renderPath ps)] [ H.span [] [ H.text x ] ]
    renderMenuItem (MISelected   x ps False) = H.a [A.class_ "current-menu-item",         A.href (renderPath ps)] [ H.span [] [ H.text x ] ]
    renderMenuItem (MIUnselected x ps True)  = H.a [A.class_ "menu-item-special",         A.href (renderPath ps)] [ H.span [] [ H.text x ] ]
    renderMenuItem (MIUnselected x ps False) = H.a [A.class_ "menu-item",                 A.href (renderPath ps)] [ H.span [] [ H.text x ] ]

    renderLock :: Sink Cmd -> Lock -> Html
    renderLock cmdU Locked       = 
      H.button [A.class_ "locked",   E.click $ const $ cmdU CUnlock] 
               [ H.img [A.src "https://image.flaticon.com/icons/svg/121/121685.svg"] [] ]
    renderLock cmdU (Unlocked _) = 
      H.button [A.class_ "unlocked", E.click $ const $ cmdU CLock]   
               [ H.img [A.src "https://image.flaticon.com/icons/svg/121/121684.svg"] [] ]

    renderGistFiles :: [File] -> Html
    renderGistFiles [] = H.div [] []
    renderGistFiles as = H.div [] (join $ fmap renderFileH as)
              
    renderFileH :: File -> [Html]
    renderFileH f = htmlStringToVirtualDom $ f_content f

    renderBlogIndex :: Area -> BlogIndex -> Html
    renderBlogIndex area bidx = H.div [A.class_ "section page"] 
                                      [ H.div [A.class_ "text"] 
                                              [ H.ul [ A.class_ "articles-index" ] 
                                                      (join $ fmap (yearsW $ blogSlug area) idx') ] ]
      where
        idx  = reverse . sortOn year . unblog $ bidx

        idx' :: [(Int, [BlogRecord])]
        idx' = foldl' folder [] idx

        folder :: [(Int, [BlogRecord])] -> BlogRecord -> [(Int, [BlogRecord])]
        folder [] x = [(year x, [x])]
        folder a  x =
          let y        = year x
              (y', xs) = last a
          in if y == y' then init a <> [(y, xs <> [x])]
                        else a <> [(y, [x])]

        yearsW :: Url -> (Int, [BlogRecord]) -> [Html]
        yearsW bs (y, xs) = 
          [ H.li [ A.class_ "articles-index article-index-year" ] 
                [ H.div [ A.class_ "article-index-year-inner"] [ H.text $ showJS y] ] 
          ] <> fmap (monthsW bs) (reverse . sortOn (\x -> fromGregorian (fromIntegral $ year x) (month x) (day x)) . Prelude.filter isPublic $ xs)

        monthsW :: Url -> BlogRecord -> Html
        monthsW bs x = 
          H.li [ A.class_ "articles-index" ]
              [ H.div [ A.class_ "article-index-title" ]
                      [ H.span [ A.class_ "article-index-title-date" ] 
                               [ H.text $ showJS (day x) <> "/" <> showJS (month x) ] 
                      , H.a [ A.href $ renderPath [bs, slug x] ] [ H.text $ humanTitle x ]
                      ]
              ]
          
