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


data BodyState = GistPending (Maybe Path) | GistError DatasourceError | GistReady Gist 
               | Blog BlogIndex BlogGist | AMessage JSS.JSString

data Cmd = CLock | CUnlock | CEdit EditCmd 


siteComponent :: SiteConfig -> FRP (Signal Html)
siteComponent c = do
  (rootU, rootS)         <- newSignal Nothing
  navS                   <- navComponent 
  (nv, nU)               <- notificationsComponent []
  (lockU, lockS)         <- newSignal Locked
  (blogU, blogS)         <- newSignal Nothing :: FRP (Sink (Maybe BlogIndexFull), Signal (Maybe BlogIndexFull)) 
  (uiToggleU, uiToggleS) <- newSignal Site
  (lv, le)               <- loginComponent nU uiToggleU :: FRP (Signal Html, Events AuthKey)
  (ev, edU, ee)          <- editorComponent nU uiToggleU lockS :: FRP (Signal Html, Sink EditCmd, Events EditResult)
  (bodyU, bodyModel)     <- newSignal (GistPending Nothing)
  (menuU, menuModel)     <- newSignal [] :: FRP (Sink (DT.Forest Page), Signal (DT.Forest Page))
  (cmdU, cmdE)           <- newEvent :: FRP (Sink Cmd, Events Cmd)

  let blogM = (,)   <$> blogS     <*> navS :: Signal (Maybe BlogIndexFull, Path)
  let pageM = (,)   <$> menuModel <*> navS :: Signal Model_                                                 
  let menuM = (,,,) <$> menuModel <*> navS <*> lockS <*> rootS :: Signal Model                                                 

  let menuV = renderMenu cmdU <$> menuM
  let lockV = renderLock cmdU <$> lockS
  let v     = view cmdU <$> bodyModel <*> lockS
  let v'    = layout <$> uiToggleS <*> v <*> lv <*> ev <*> nv <*> menuV <*> lockV
  
  void $ subscribeEvent (updates navS) handleRedirects
  void $ titleComponent $ (,) <$> menuM <*> blogS
  void $ subscribeEvent (updates blogM) $ handleBlogPage lockS                   bodyU
  void $ subscribeEvent (updates pageM) $ handleTreePage lockS                   bodyU
  void $ subscribeEvent ee              $ handleEdits    lockS rootU blogU menuU bodyU
  void $ subscribeEvent cmdE            $ handleCmd      uiToggleU lockU edU
  void $ subscribeEvent le              $ handleLogin    uiToggleU lockU

  void . forkIO $ loadMenu      lockS (rootGist c) rootU menuU bodyU 
  void . forkIO $ loadBlogIndex lockS (blogGist c) blogU       bodyU

  pure v'

  where 
    layout :: ViewMode -> Html -> Html -> Html -> Html -> Html -> Html -> Html
    layout s bv lv ev nv mv kv = case s of
      Site   -> H.div [] [nv, wrapper mv kv bv]
      Login  -> H.div [] [nv, wrapper' lv]
      Editor -> H.div [] [nv, overlayWrapper ev]

    handleRedirects :: Path -> IO ()
    handleRedirects p = case redirects p of
      Just p' -> redirectLocal p'
      Nothing -> pure ()

    handleLogin :: Sink ViewMode -> Sink Lock -> AuthKey -> FRP ()
    handleLogin uiToggleU lockU authkey = uiToggleU Site >> lockU (Unlocked authkey)

    handleEdits :: Signal Lock -> Sink (Maybe RootGist) -> Sink (Maybe BlogIndexFull) -> Sink (DT.Forest Page) -> Sink BodyState -> EditResult -> FRP ()
    handleEdits lockS rootU _     menuU  bodyU (RRootGist rg) = loadMenu      lockS (Types.id . digout $ rg)  rootU menuU bodyU 
    handleEdits lockS _     blogU _      bodyU (RBlogGist bg) = loadBlogIndex lockS (Types.id . blogout $ bg) blogU        bodyU 
    handleEdits lockS _     _     _      bodyU (RGist g)      = loadGist_ lockS bodyU (Types.id g) $ bodyU . GistReady                                -- really, navTo gist url?
    handleEdits _     _     _     _      bodyU (RNew g)       = bodyU . AMessage $ "Your new gist has been created, id = " <> getGistId (Types.id g)

    handleCmd :: Sink ViewMode -> Sink Lock -> Sink EditCmd -> Cmd -> FRP ()
    handleCmd uiToggleU lockU edU cmd = 
      case cmd of
        CLock   -> uiToggleU Site >> lockU Locked
        CUnlock -> uiToggleU Login
        CEdit g -> edU g

    handleBlogPage :: Signal Lock -> Sink BodyState -> (Maybe BlogIndexFull, Path) -> FRP ()
    handleBlogPage lockS bodyU (bi, p:ps) | isBlog (p:ps) = case (bi, ps) of
      (Nothing,         [])     -> bodyU . GistPending . Just $ p:ps -- blog index
      (Nothing,         _:[])   -> bodyU . GistPending . Just $ p:ps -- blog article
      (Just (bi', big), [])     -> bodyU $ Blog bi' big
      (Just (bi', _),   bid:[]) -> loadBlog lockS bodyU bi' bid
      _                         -> pure ()
    
    handleBlogPage _ _ _ = pure ()

    handleTreePage :: Signal Lock -> Sink BodyState -> Model_ -> FRP ()
    handleTreePage _     _     (_, p) | isBlog p = pure ()
    handleTreePage lockS bodyU (f, p)            = case (f, p) of
      ([], [])  -> bodyU $ GistPending Nothing
      ([], p')  -> bodyU . GistPending . Just $ p'
      (m:_, []) -> loadGist_ lockS bodyU (dataSource . DT.rootLabel $ m) $ bodyU . GistReady -- home page
      (ms, ps)  -> case findTreeByPath ms ps of
                     Nothing   -> bodyU . GistError . NotFound $ ps
                     Just page -> loadGist_ lockS bodyU (dataSource page) $ bodyU . GistReady
    
    loadBlog :: Signal Lock -> Sink BodyState -> BlogIndex -> Url -> FRP ()
    loadBlog lockS bodyU (BlogIndex bi) s = do
      bodyU . GistPending . Just $ [s]
      case listToMaybe (Prelude.filter ((s ==) . slug) bi) <|> 
           listToMaybe (Prelude.filter ((s ==) . hash) bi) of
        Nothing -> bodyU . GistError . NotFound $ [s]
        Just x  -> loadGist_ lockS bodyU (GistId $ hash x) $ bodyU . GistReady
    
    loadBlogIndex :: Signal Lock -> GistId -> Sink (Maybe BlogIndexFull) -> Sink BodyState -> FRP ()
    loadBlogIndex lockS bg blogU bodyU  = 
      loadGist_ lockS bodyU bg $ \blogGist_ -> 
        case unfiles $ files blogGist_ of
          []    -> bodyU $ GistError $ DatasourceError "There are no files in this forest"
          (f:_) -> let forest = eitherDecodeStrict' . TE.encodeUtf8 . T.pack . JSS.unpack . f_content $ f :: Either String BlogIndex
                   in case forest of
                          Left err -> bodyU $ GistError $ DatasourceError $ JSS.pack err
                          Right bi' -> blogU $ Just (bi', BlogGist blogGist_)
    
    loadMenu :: Signal Lock -> GistId -> Sink (Maybe RootGist) 
             -> Sink (DT.Forest Page) -> Sink BodyState -> FRP ()
    loadMenu lockS rg rootU menuU bodyU  = 
      loadGist_ lockS bodyU rg $ \rootGist_ -> do
        rootU $ Just $ RootGist rootGist_
        case unfiles $ files rootGist_ of
          []    -> bodyU $ GistError $ DatasourceError "There are no files in this forest"
          (f:_) -> let forest = eitherDecodeStrict' . TE.encodeUtf8 . T.pack . JSS.unpack . f_content $ f :: Either String (DT.Forest Page)
                   in case forest of
                          Left err      -> bodyU $ GistError $ DatasourceError $ JSS.pack err
                          Right forest' -> menuU forest'

    findTreeByPath :: DT.Forest Page -> Path -> Maybe Page
    findTreeByPath f p = case (f, p) of
      ([],_)     -> Nothing
      (x:_, [])  -> Just $ DT.rootLabel x
      (xs, y:ys) -> case Prelude.filter ((y ==) . path . DT.rootLabel) xs of
                      []  -> Nothing
                      x:_ -> case ys of
                                []  -> Just $ DT.rootLabel x
                                ys' -> findTreeByPath (DT.subForest x) ys'

    loadGist_ :: Signal Lock -> Sink BodyState -> GistId -> (Gist -> IO ()) -> IO ()
    loadGist_ lockS bodyU g f = void . forkIO $ do
      bodyU $ GistPending Nothing
      k <- pollBehavior $ current lockS
      a <- loadGist k g :: IO (Either DatasourceError ApiResult)
      case a of
        Left x   -> bodyU $ GistError x
        Right a' -> case a' of
                      Left  m   -> bodyU $ GistError $ DatasourceError (message m)
                      Right a'' -> f a''

    view :: Sink Cmd -> BodyState -> Lock -> Html
    view cmdU (GistPending p) _ = 
      let ps = fromMaybe "" $ renderPath <$> p
      in H.div [A.class_ "loader-container"] 
               [ H.div [] [H.text $ "Loading " <> ps]
               , H.img [A.class_ "ajax-loader", A.src "img/ajax-loader.gif"] [] ]

    view cmdU (GistError (DatasourceError s)) _ = 
      H.div [A.class_ "s500"] 
            [ -- H.span [A.class_ "error-description"] [H.text "Error fetching data: "]
              H.span [A.class_ "error-message"] [H.text s ]
            -- , H.span [A.class_ "error-sorry"] [H.text " Sorry for that."]
            ]
                   
    view cmdU (GistError (NotFound ps)) _ = 
      H.div [A.class_ "s404"] 
            [ H.text "The path "
            , H.span [A.class_ "path"] [H.text $ renderPath ps ]
            , H.text " was not found in this forest."
            ]
    
    view cmdU (AMessage msg) _ = H.div [] [ H.text msg ]
    
    view cmdU (Blog bi big) k = 
      H.div [] (editBlogButton cmdU big k <> createButton cmdU k <> [renderBlogIndex bi])

    view cmdU (GistReady g) k = 
      H.div [] (editButton cmdU (Right g) k <> createButton cmdU k <> [renderGistFiles . unfiles . files $ g])
    
    wrapper :: Html -> Html -> Html -> Html
    wrapper menuH lockH bodyH = 
      H.div [A.class_ "content"]
            [ H.div [A.class_ "lock"] [lockH]
            , H.div [A.class_ "section"] $ [menuH, bodyH]]

    wrapper' :: Html -> Html
    wrapper' b = 
      H.div [A.class_ "content"]
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
    
    overlayWrapper :: Html -> Html
    overlayWrapper b = 
      H.div [A.class_ "content overlay"]
            [H.div [A.class_ "section"] [ b ]]

    renderMenu :: Sink Cmd -> Model -> Html
    renderMenu cmdU (f, p, Locked, _)         = let m = extractMenu f p [] in H.div [A.class_ "nav"] (renderSubMenu p 0 m)
    renderMenu cmdU (f, p, k@(Unlocked _), r) = let m = extractMenu f p [] in H.div [A.class_ "nav"] (renderSubMenu p 0 m <> editButton cmdU (Left r) k)

    renderSubMenu :: Path -> Int -> Menu -> [Html]
    renderSubMenu _ _ MenuNil                       = []
    renderSubMenu _ _ (Menu (MenuLevel []) _)       = []
    renderSubMenu p l (Menu (MenuLevel xs) MenuNil) = renderMenuLevel p l xs
    renderSubMenu p l (Menu (MenuLevel xs) sm)      = renderMenuLevel p l xs <> renderSubMenu p (l+1) sm

    renderMenuLevel :: Path -> Int -> [MenuItem] -> [Html]
    renderMenuLevel _ _   []                  = []
    renderMenuLevel z 0   m | isSpecialPath z = [H.div [A.class_ $ "menu-level menu-level-special menu-level-" <> showJS 0]   (fmap renderMenuItem m)]
    renderMenuLevel _ lvl m                   = [H.div [A.class_ $ "menu-level                    menu-level-" <> showJS lvl] (fmap renderMenuItem m)]

    renderMenuItem :: MenuItem -> Html
    renderMenuItem (MISelected x ps True)    = H.a [A.class_ "current-menu-item-special", A.href (renderPath ps)] [ H.text x ]
    renderMenuItem (MISelected x ps False)   = H.a [A.class_ "current-menu-item",         A.href (renderPath ps)] [ H.text x ]
    renderMenuItem (MIUnselected x ps True)  = H.a [A.class_ "menu-item-special",         A.href (renderPath ps)] [ H.text x ]
    renderMenuItem (MIUnselected x ps False) = H.a [A.class_ "menu-item",                 A.href (renderPath ps)] [ H.text x ]

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

    renderBlogIndex :: BlogIndex -> Html
    renderBlogIndex bidx = H.div [A.class_ "section page"] 
                                 [ H.div [A.class_ "text"] 
                                         [ H.ul [ A.class_ "articles-index" ] 
                                                (join $ fmap yearsW idx') ] ]
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

        yearsW :: (Int, [BlogRecord]) -> [Html]
        yearsW (y, xs) = 
          [ H.li [ A.class_ "articles-index article-index-year" ] 
                [ H.div [ A.class_ "article-index-year-inner"] [ H.text $ showJS y] ] 
          ] <> fmap monthsW (reverse . sortOn (\x -> fromGregorian (fromIntegral $ year x) (month x) (day x)) $ xs)

        monthsW :: BlogRecord -> Html
        monthsW x = 
          H.li [ A.class_ "articles-index" ]
              [ H.div [ A.class_ "article-index-title" ]
                      [ H.span [ A.class_ "article-index-title-date" ] 
                               [ H.text $ showJS (day x) <> "/" <> showJS (month x) ] 
                      , H.a [ A.href $ renderPath [blogSlug, slug x] ] [ H.text $ humanTitle x ]
                      ]
              ]
          
