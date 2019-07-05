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


titleComponent :: Signal (Model, Maybe BlogIndexFull) -> FRP ()
titleComponent m = do
  let m' = fmap handleTitle m
  void $ subscribeEvent (updates m') setTitle

  where
    handleTitle z@(((Area bs _ r s _), p:bid:etc, _, _), _) | isBlog bs (p:bid:etc) = blogPipeline r s z bid
    handleTitle z@(((Area _  _ r s _), _,         _, _), _)                         = menuPipeline r s z

    menuPipeline r s = JSS.intercalate s . reverse . (r :) . fmap getTitle . flattenMenu . extractMenu' . fst

    blogPipeline r s z@(_, mbi) bid = 
      case mbi of
        Nothing      -> menuPipeline r s z
        Just (bi, _) -> 
          let br = listToMaybe (Prelude.filter ((bid ==) . slug) (unblog bi)) <|> 
                   listToMaybe (Prelude.filter ((bid ==) . hash) (unblog bi)) 
              t  = menuPipeline r s z
              bt = fromMaybe bid $ humanTitle <$> br
              ft = JSS.intercalate s [bt, t]
          in ft

    extractMenu' ((Area _ _ _ _ f), p, _, _) = extractMenu f p []

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
