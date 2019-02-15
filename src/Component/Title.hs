{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Title
    ( titleComponent
    ) where

import           GHCJS.Types                    (JSString)
import           GHCJS.Foreign.Callback
import           Data.Aeson
import qualified Data.ByteString.Char8          as BS
import qualified Data.JSString                  as JSS      
import           Data.Maybe                     (listToMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Tree                      as DT
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void, join)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A    
import qualified Web.VirtualDom.Html.Events     as E    

import           Lubeck.App                     (Html, KbdEvents(..))
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)
import           Lubeck.Web.URI                 (decodeURIComponent)

import           Net
import           Types
import           Lib
import           UICombinators


titleComponent :: Signal Model -> FRP ()
titleComponent s = do
  let s' = fmap (JSS.intercalate " ← " . reverse . ("EN" :) . fmap getTitle . flattenMenu . extractMenu') s
  void $ subscribeEvent (updates s') setTitle

  where
    extractMenu' (f, p, _) = extractMenu f p []

    flattenMenu MenuNil          = []
    flattenMenu (Menu m MenuNil) = findSelectedItem m
    flattenMenu (Menu m sm)      = findSelectedItem m <> flattenMenu sm

    findSelectedItem (MenuLevel xs) = Prelude.filter isSelected xs

    isSelected (MISelected   _ _) = True
    isSelected (MIUnselected _ _) = False
    
    getTitle (MISelected   x _) = x
    getTitle (MIUnselected x _) = x

foreign import javascript unsafe "document.title = $1"
  setTitle :: JSString -> IO ()
