{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lubeck.App     (runAppReactive)
import           Lib            (siteComponent)
import           Types
import           Utils


c :: SiteConfig
c = SiteConfig (GistId "2f741d60ab90d986379aa63658f0300e")

main :: IO ()
main = do
  (v, ku) <- siteComponent c
  kbdListener $ ku . toEvent 

  runAppReactive v
