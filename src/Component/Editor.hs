{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Editor
    ( editorComponent
    ) where

import           GHCJS.Types                    (JSString)
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)
import qualified Data.JSString                  as JSS   
import           Data.Monoid                    ((<>))
import           Data.Maybe                     (listToMaybe)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A    
import qualified Web.VirtualDom.Html.Events     as E      

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Forms                     
import           Lubeck.Util                    (showJS)

import           Lib
import           Net
import           Types
import           UICombinators


type EditForm = (Gist, File, JSString)

data CType = None | RootG | JustG

emptyForm :: EditForm
emptyForm = (Gist Nothing Nothing (GistId "") "" (Files []), File "" "" "" 0 Plaintext, "")

editorComponent :: Sink ViewMode -> Signal Lock -> FRP (Signal Html, Sink (Either RootGist Gist), Events (Either RootGist Gist))
editorComponent uiToggleU lockS = do
  (inpU, inpE)   <- newEvent :: FRP (Sink (Either RootGist Gist), Events (Either RootGist Gist))
  (tU, tS)       <- newSignal None
  (outpU, outpE) <- newEvent
  (v, e, reset)  <- formC emptyForm (w uiToggleU)

  void $ subscribeEvent inpE $ \xg -> do
    g <- case xg of
            Left  x -> tU RootG >> pure (digout x)
            Right y -> tU JustG >> pure y
    let fs = files g
        f = listToMaybe $ unfiles fs
    
    case f of
      Nothing -> print "Bad gist" 
      Just f' -> do
        reset (g, f', f_content f')
        uiToggleU Editor

  void $ subscribeEvent e $ \(g, f, c) -> void . forkIO $ do
    let c' = JSS.replace "</p>" "" . JSS.replace "<p>" "" $ c -- FIXME ckeditor keeps wrapping content into <p>..</p>
        f' = f{f_content = c'}
        g' = g{files = Files [f']}

    a <- pollBehavior $ current lockS
    case a of
      Locked -> print "Not logged in" >> pure ()
      Unlocked ak -> do
        r <- saveGist_ ak g'
        case r of
          Right _ -> do
            t <- pollBehavior $ current tS
            case t of
              None  -> print "Wrong editor state None"
              RootG -> reset emptyForm >> uiToggleU Site >> outpU (Left $ RootGist g)
              JustG -> reset emptyForm >> uiToggleU Site >> outpU (Right g)
          Left x  -> print $ "Error saving gist: " <> showJS x

  pure (v, inpU, outpE)

  where
    saveGist_ :: AuthKey -> Gist -> IO (Either DatasourceError Gist)
    saveGist_ ak g = do
      r <- patchAPI api (getGistId $ Types.id g) g
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
      aS <- stepperS (DontSubmit z) aEvent
      let htmlS = fmap (widget aSink . submitValue) aS
      let reset = aSink . DontSubmit
      pure (htmlS, submits aEvent, reset)

    w :: Sink ViewMode ->  Widget EditForm (Submit EditForm)
    w loginToggleU_ u (g, f, c) =  
      H.div [A.class_ "editor-form"] 
            [ H.button [E.click $ \_ -> u $ Submit (g, f, c)] [H.text "Save"]
            , H.button [E.click $ \_ -> u (DontSubmit emptyForm) >> loginToggleU_ Site] [H.text "Cancel"]
            , richEditorWidget True (contramapSink (\n -> DontSubmit (g, f, n)) u) c
            ]
