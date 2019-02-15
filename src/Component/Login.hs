{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Login
    ( loginComponent
    ) where

import           GHCJS.Types                    (JSString)
import           Control.Monad                  (void)
import           Data.Monoid                    ((<>))
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


data LoginForm = 
  LoginForm 
    { username :: JSString
    , password :: JSString
    }
data FormValid = FormValid AuthKey | FormNotValid DatasourceError

emptyForm :: LoginForm
emptyForm = LoginForm "" ""

loginComponent :: Sink ViewMode -> FRP (Signal Html, Events AuthKey)
loginComponent loginToggleU = do
  (u, xe) <- newEvent
  (v, e, reset) <- formC emptyForm (w loginToggleU)

  void $ subscribeEvent e $ \lp -> do
    ok <- validate lp
    case ok of
      FormValid usr  -> reset >> u usr
      FormNotValid x -> print x

  pure (v, xe)

  where
    validate :: LoginForm -> IO FormValid
    validate (LoginForm u p) = do
      r <- authenticateOrError (u, p)
      pure $ case r of
        Left e  -> FormNotValid e
        Right usr -> FormValid $ AuthKey u p usr

    
    authenticateOrError :: (JSString, JSString) -> IO (Either DatasourceError GithubUser)
    authenticateOrError (unm, psw) = do
      res <- getAPI api "" :: IO (Either DatasourceError GithubUser)
      pure res

      where
        api = userApi { headers = [authHeader] }
        authHeader = ("Authorization", "Basic " <> base64encode (unm <> ":" <> psw))
        base64encode = btoa

    formC ::  a -> Widget a (Submit a) -> IO (Signal Html, Events a, FRP ())
    formC z widget = do
      (aSink, aEvent) <- newEvent :: IO (Sink (Submit a), Events (Submit a))
      aS <- stepperS (DontSubmit z) aEvent
      let htmlS = fmap (widget aSink . submitValue) aS
      let reset = aSink $ DontSubmit z
      pure (htmlS, submits aEvent, reset)

    w :: Sink ViewMode ->  Widget LoginForm (Submit LoginForm)
    w loginToggleU u v@(LoginForm uname pass) =  
      H.div [A.class_ "login-form"] 
            [ H.h1 [] [H.text "Login"]
            , H.div [] [stringWidget True  (contramapSink (\n -> DontSubmit $ LoginForm n pass)  u) uname]
            , H.div [] [stringWidget False (contramapSink (\n -> DontSubmit $ LoginForm uname n) u) pass]
            , H.button [E.click $ \_ -> u $ Submit v] [H.text "Dare"]
            , H.button [E.click $ \_ -> loginToggleU Site] [H.text "Sorry"]
            ]
