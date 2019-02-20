{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Editor
    ( editorComponent
    , EditCmd(..)
    , EditResult(..)
    ) where

import           GHCJS.Types                    (JSString)
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)
import qualified Data.JSString                  as JSS   
import           Data.Monoid                    ((<>))
import           Data.Maybe                     (listToMaybe)
import           Data.Time.Clock                (getCurrentTime)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A    
import qualified Web.VirtualDom.Html.Events     as E      

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Forms                     
import           Lubeck.Util                    (showJS, newSyncEventOf)

import           Lib
import           Net
import           Types
import           UICombinators
import           Component.Notification         (Notification, nerr)


type EditForm = (Gist, File, JSString)

data CType = None | RootG | JustG | NewG

data Busy = Busy | Idle
data CloseState = Close | DontClose

data EditCmd    = ERootGist RootGist | EGist Gist | ECreate
data EditResult = RRootGist RootGist | RGist Gist | RNew Gist

emptyFile :: File
emptyFile = File "" "" "" 0 Plaintext

emptyGist :: Gist
emptyGist = Gist Nothing Nothing (GistId "") "" (Files [emptyFile]) False

emptyForm :: EditForm
emptyForm = (emptyGist, emptyFile, "")

editorComponent :: Sink (Maybe Notification) -> Sink ViewMode -> Signal Lock -> FRP (Signal Html, Sink EditCmd, Events EditResult)
editorComponent nU uiToggleU lockS = do
  (inpU, inpE)     <- newSyncEventOf (undefined :: EditCmd)
  (outpU, outpE)   <- newSyncEventOf (undefined :: EditResult) 
  (tU, tS)         <- newSignal None
  (busyU, busyS)   <- newSignal Idle
  (closeU, closeS) <- newSignal Close
  (v, e, reset)    <- formC emptyForm (w closeU uiToggleU)
  let busyV        = fmap busyW busyS
  let v'           = layout <$> v <*> busyV

  void $ subscribeEvent inpE $ handleInputCmd tU reset
  void $ subscribeEvent e $ handleEditSubmit closeS tS lockS reset busyU outpU

  pure (v', inpU, outpE)

  where
    handleInputCmd :: Sink CType -> (EditForm -> FRP ()) -> EditCmd -> FRP ()
    handleInputCmd tU reset cmd = do
      g <- case cmd of
              ERootGist  x -> tU RootG >> pure (digout x)
              EGist y      -> tU JustG >> pure y
              ECreate      -> tU NewG  >> pure emptyGist
      
      case listToMaybe . unfiles . files $ g of
        Nothing -> error_ ("Bad gist" :: JSString) 
        Just f' -> reset (g, f', f_content f') >> uiToggleU Editor
  
    handleEditSubmit :: Signal CloseState -> Signal CType -> Signal Lock -> (EditForm -> FRP ()) 
                     -> Sink Busy -> Sink EditResult -> (Gist, File, JSString) 
                     -> IO ()
    handleEditSubmit closeS tS lockS reset busyU outpU (g, f, c) = void . forkIO $ do
      a <- pollBehavior $ current lockS
      t <- pollBehavior $ current tS
  
      now <- getCurrentTime
  
      let c' = case t of
                  RootG -> JSS.dropEnd 4 . JSS.drop 3 $ c -- FIXME ckeditor keeps wrapping content into <p>..</p>
                  _     -> c
          d  = case t of
                  NewG -> showJS now <> ".html"
                  _    -> description g
          f' = f{f_content = c'}
          g' = g{files = Files [f'], description = d}

      case (a, t) of
        (Locked, _)         -> error_ ("Not logged in" :: JSString) >> pure ()
        (Unlocked _,  None) -> error_ ("Wrong editor state None" :: JSString)
        (Unlocked ak, NewG) -> createGist_ busyU ak g' >>= handleResult closeS reset uiToggleU outpU t 
        (Unlocked ak, _)    -> saveGist_   busyU ak g' >>= handleResult closeS reset uiToggleU outpU t 

    error_ :: JSString -> FRP ()
    error_ = nU . Just . nerr

    layout :: Html -> Html -> Html
    layout v bv = H.div [] [bv, v]

    busyW :: Busy -> Html
    busyW Idle = H.div [A.class_ "loader-container"] []
    busyW Busy = H.div [A.class_ "loader-container-editor"] 
                       [ H.img [A.class_ "ajax-loader", A.src "img/ajax-loader.gif"] [] ]

    handleResult :: Signal CloseState -> (EditForm -> FRP ()) -> Sink ViewMode -> Sink EditResult ->  CType 
                  -> Either DatasourceError Gist -> FRP ()
    handleResult closeS reset uiToggleU_ outpU t (Right g) = case t of
          None  -> error_ ("Wrong editor state None" :: JSString)
          RootG -> closeOrNot reset closeS uiToggleU_ >> outpU (RRootGist $ RootGist g)
          JustG -> closeOrNot reset closeS uiToggleU_ >> outpU (RGist g)
          NewG  -> closeOrNot reset closeS uiToggleU_ >> outpU (RNew g)
    handleResult _ _ _ _ _ (Left x) = error_ $ "Error saving gist: " <> showJS x

    closeOrNot reset closeS uiToggleU_ = do
      x <- pollBehavior $ current closeS
      case x of
        Close     -> reset emptyForm >> uiToggleU_ Site
        DontClose -> pure ()
    
    saveGist_ :: Sink Busy -> AuthKey -> Gist -> IO (Either DatasourceError Gist)
    saveGist_ busyU ak g = do
      busyU Busy 
      r <- patchAPI api ("/" <> getGistId (Types.id g)) g
      busyU Idle
      pure r

      where
        unm  = username ak
        psw  = password ak
        api  = gistApi { headers = [auth, ct] }
        auth = ("Authorization", "Basic " <> base64encode (unm <> ":" <> psw))
        ct   = ("Content-Type", "application/json")
        base64encode = btoa

    createGist_ :: Sink Busy -> AuthKey -> Gist -> IO (Either DatasourceError Gist)
    createGist_ busyU ak g = do
      busyU Busy 
      r <- postAPI api "" g
      busyU Idle
      pure r

      where
        unm  = username ak
        psw  = password ak
        api  = gistApi { headers = [auth, ct] }
        auth = ("Authorization", "Basic " <> base64encode (unm <> ":" <> psw))
        ct   = ("Content-Type", "application/json")
        base64encode = btoa

    formC ::  a -> Widget a (Submit a) -> IO (Signal Html, Events a, a -> FRP ())
    formC z widget = do
      (aSink, aEvent) <- newEvent :: IO (Sink (Submit a), Events (Submit a))
      aS              <- stepperS (DontSubmit z) aEvent
      let htmlS = fmap (widget aSink . submitValue) aS
      let reset = aSink . DontSubmit
      pure (htmlS, submits aEvent, reset)

    w :: Sink CloseState -> Sink ViewMode ->  Widget EditForm (Submit EditForm)
    w closeU loginToggleU_ u (g, f, c) =  
      H.div [A.class_ "editor-form"] 
            [ H.button [E.click $ \_ -> closeU DontClose >> u (Submit (g, f, c))] [H.text "Save"]
            , H.button [E.click $ \_ -> closeU Close >> u (Submit (g, f, c))] [H.text "Save and close"]
            , H.button [E.click $ \_ -> closeU Close >> u (DontSubmit emptyForm) >> loginToggleU_ Site] [H.text "Cancel"]
            , richEditorWidget True (contramapSink (\n -> DontSubmit (g, f, n)) u) c
            ]
