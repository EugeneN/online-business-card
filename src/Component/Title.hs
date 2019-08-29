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


titleComponent :: Signal Model -> FRP ()
titleComponent m = do
  let m' = fmap handleTitle m
  void $ subscribeEvent (updates m') setTitle

  where
    handleTitle z@((Area bs _ _ _ _ _), p:bid:etc, _) | isBlog bs (p:bid:etc) = blogPipeline z bid
    handleTitle z                                                           = menuPipeline z

    menuPipeline z@((Area _ _ r s _ _), _, _) = JSS.intercalate s . reverse . (r :) . fmap getTitle . flattenMenu . extractMenu' $ z

    blogPipeline z@((Area _ _ _ s _ _), _, mbi) bid = 
      case mbi of
        Nothing      -> menuPipeline z
        Just (bi, _) -> 
          let br = listToMaybe (Prelude.filter ((bid ==) . slug) (unblog bi)) <|> 
                   listToMaybe (Prelude.filter ((bid ==) . hash) (unblog bi)) 
              t  = menuPipeline z
              bt = fromMaybe bid $ humanTitle <$> br
              ft = JSS.intercalate s [bt, t]
          in ft

    extractMenu' ((Area _ _ _ _ f _), p, _) = extractMenu f p []

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
