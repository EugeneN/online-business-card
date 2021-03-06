{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Login
    ( loginComponent
    ) where

import           GHCJS.Types                    (JSString)
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)
import           Data.Monoid                    ((<>))
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A    
import qualified Web.VirtualDom.Html.Events     as E      

import           Lubeck.App                     (Html)
import           Lubeck.FRP                     
import           Lubeck.Forms                   hiding (passwordWidget)                  

import           Lib
import           Net
import           Types
import           UICombinators
import           Component.Notification         (Notification, nerr, nsuccess)


data LoginForm = 
  LoginForm 
    { token :: JSString }
data FormValid = FormValid AuthKey | FormNotValid DatasourceError

emptyForm :: LoginForm
emptyForm = LoginForm ""

loginComponent :: Sink (Maybe Notification) -> Sink ViewMode -> FRP (Signal Html, Events AuthKey)
loginComponent nU uiToggleU = do
  (u, xe) <- newEvent
  (v, e, reset) <- formC emptyForm (w uiToggleU)

  void $ subscribeEvent e $ \lp -> void . forkIO $ do
    ok <- validate lp
    case ok of
      FormValid usr  -> reset >> nU (Just $ nsuccess "Welcome") >> u usr
      FormNotValid x -> nU . Just . nerr . toString $ x

  pure (v, xe)

  where
    toString :: DatasourceError -> JSString
    toString (DatasourceError s) = "DatasourceError: " <> s 
    toString (NotFound p)        = "Path not found: " <> renderPath p

    validate :: LoginForm -> IO FormValid
    validate (LoginForm tok) = do
      r <- authenticateOrError (tok)
      pure $ case r of
        Left e    -> FormNotValid e
        Right usr -> FormValid $ AuthKey tok usr
    
    authenticateOrError :: JSString -> IO (Either DatasourceError GithubUser)
    authenticateOrError tok = do
      getAPI api "" :: IO (Either DatasourceError GithubUser)

      where
        api  = userApi { headers = [auth, ct] }
        auth = ("Authorization", "token " <> tok)
        ct   = ("Content-Type", "application/json")
        base64encode = btoa

    formC ::  a -> Widget a (Submit a) -> IO (Signal Html, Events a, FRP ())
    formC z widget = do
      (aSink, aEvent) <- newEvent :: IO (Sink (Submit a), Events (Submit a))
      aS <- stepperS (DontSubmit z) aEvent
      let htmlS = fmap (widget aSink . submitValue) aS
      let reset = aSink $ DontSubmit z
      pure (htmlS, submits aEvent, reset)

    w :: Sink ViewMode ->  Widget LoginForm (Submit LoginForm)
    w uiToggleU' u v@(LoginForm tok) =  
      H.div [A.class_ "login-form"] 
            [ H.h1 [] [H.text "Login"]
            , H.div [] [passwordWidget False (contramapSink (\n -> DontSubmit $ LoginForm n) u) tok]
            , H.button [E.click $ \_ -> u $ Submit v] [H.text "Okay"]
            , H.button [E.click $ \_ -> uiToggleU' Site] [H.text "Nope"]
            ]
