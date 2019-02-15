{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Login
    ( loginComponent
    ) where

import           GHCJS.Types                    (JSString)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A    
import qualified Web.VirtualDom.Html.Events     as E      

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Forms                     

import           Types
import           UICombinators


data LoginForm = LoginForm JSString
data FormValid = FormValid | FormNotValid [JSString]

emptyForm :: LoginForm
emptyForm = LoginForm ""

loginComponent :: Sink ViewMode -> FRP (Signal Html, Events JSString)
loginComponent loginToggleU = do
  (u, xe) <- newEvent
  (v, e, reset) <- formC emptyForm (w loginToggleU)

  _ <- subscribeEvent e $ \(LoginForm pass) -> do
    ok <- validate pass
    case ok of
      FormValid      -> reset >> u pass
      FormNotValid _ -> print "Nope."

  pure (v, xe)

  where
    validate :: JSString -> IO FormValid
    validate x =  pure $ if x == "zzz" then FormValid else FormNotValid ["Nope."]

    formC ::  a -> Widget a (Submit a) -> IO (Signal Html, Events a, FRP ())
    formC z w = do
      (aSink, aEvent) <- newEvent :: IO (Sink (Submit a), Events (Submit a))
      aS <- stepperS (DontSubmit z) aEvent
      let htmlS = fmap (w aSink . submitValue) aS
      let reset = aSink $ DontSubmit z
      pure (htmlS, submits aEvent, reset)

    w :: Sink ViewMode ->  Widget LoginForm (Submit LoginForm)
    w loginToggleU u v@(LoginForm s) =  
      H.div [A.class_ "login-form"] 
            [ H.h1 [] [H.text "Login"]
            , stringWidget True (contramapSink (DontSubmit . LoginForm) u) s
            , H.button [E.click $ \_ -> u $ Submit v] [H.text "Dare"]
            , H.button [E.click $ \_ -> loginToggleU Site] [H.text "Sorry"]
            ]
