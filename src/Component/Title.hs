{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Title
    ( titleComponent
    ) where

import           GHCJS.Types                    (JSString)
import           Control.Applicative            ((<|>))
import           Control.Monad                  (void)
import qualified Data.JSString                  as JSS   
import           Data.Maybe                     (listToMaybe, fromMaybe)   
import           Data.Monoid                    ((<>))

import           Lubeck.FRP                     

import           Types
import           Lib

sep :: JSString
sep = " ← "

root :: JSString
root = "ΞN"

titleComponent :: Signal (Model, Maybe BlogIndexFull) -> FRP ()
titleComponent s = do
  let s' = fmap handleTitle s
  void $ subscribeEvent (updates s') setTitle

  where
    handleTitle z@((_, p:bid:etc, _, _), _) | isBlog (p:bid:etc) = blogPipeline z bid
    handleTitle z                                                = menuPipeline z

    menuPipeline = JSS.intercalate sep . reverse . (root :) . fmap getTitle . flattenMenu . extractMenu' . fst

    blogPipeline z@(_, mbi) bid = 
      case mbi of
        Nothing      -> menuPipeline z
        Just (bi, _) -> 
          let br = listToMaybe (Prelude.filter ((bid ==) . slug) (unblog bi)) <|> 
                   listToMaybe (Prelude.filter ((bid ==) . hash) (unblog bi)) 
              t  = menuPipeline z
              bt = fromMaybe bid $ humanTitle <$> br
              ft = JSS.intercalate sep [bt, t]
          in ft

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
