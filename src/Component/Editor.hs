{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Editor
    ( editorComponent
    ) where

import           GHCJS.Types                    (JSString)
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)
import           Data.Monoid                    ((<>))
import           Data.Maybe                     (listToMaybe)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A    
import qualified Web.VirtualDom.Html.Events     as E      

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Forms                     

import           Lib
import           Net
import           Types
import           UICombinators


type EditForm = (Gist, File, JSString)

emptyForm :: EditForm
emptyForm = (Gist Nothing Nothing (GistId "") "" (Files []), File "" "" "" 0 Plaintext, "")

editorComponent :: Sink ViewMode -> Signal Lock -> FRP (Signal Html, Sink Gist, Events Gist)
editorComponent loginToggleU lockS = do
  (inpU, inpE)   <- newEvent :: FRP (Sink Gist, Events Gist)
  (outpU, outpE) <- newEvent
  (v, e, reset)  <- formC emptyForm (w loginToggleU)

  void $ subscribeEvent inpE $ \g -> do
    let fs = files g
        f = listToMaybe $ unfiles fs
    
    case f of
      Nothing -> print "Bad gist" 
      Just f' -> do
        reset (g, f', f_content f')
        loginToggleU Editor

  void $ subscribeEvent e $ \(g, f, c) -> void . forkIO $ do
    let f' = f{f_content = c}
        g' = g{files = Files [f']}

    a <- pollBehavior $ current lockS
    case a of
      Locked -> print "Not logged in" >> pure ()
      Unlocked ak -> do
        r <- saveGist_ ak g'
        case r of
          Right _ -> reset emptyForm >> loginToggleU Site >> outpU g
          Left x  -> print x

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
