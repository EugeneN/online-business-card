{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Title
    ( titleComponent
    ) where

import           GHCJS.Types                    (JSString)
import           Control.Monad                  (void)
import qualified Data.JSString                  as JSS      
import           Data.Monoid                    ((<>))

import           Lubeck.FRP                     

import           Types
import           Lib


titleComponent :: Signal Model -> FRP ()
titleComponent s = do
  let s' = fmap (JSS.intercalate " ← " . reverse . ("EN" :) . fmap getTitle . flattenMenu . extractMenu') s
  void $ subscribeEvent (updates s') setTitle

  where
    extractMenu' (f, p, _, _) = extractMenu f p []

    flattenMenu MenuNil          = []
    flattenMenu (Menu m MenuNil) = findSelectedItem m
    flattenMenu (Menu m sm)      = findSelectedItem m <> flattenMenu sm

    findSelectedItem (MenuLevel xs) = Prelude.filter isSelected xs

    isSelected (MISelected   _ _ _) = True
    isSelected (MIUnselected _ _ _) = False
    
    getTitle (MISelected   x _ _) = x
    getTitle (MIUnselected x _ _) = x

foreign import javascript unsafe "document.title = $1"
  setTitle :: JSString -> IO ()
