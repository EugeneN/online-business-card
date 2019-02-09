{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Lib            (SiteConfig(..), GistId(..), siteComponent)
import           Lubeck.App     (runAppReactive)


c = SiteConfig (GistId "2f741d60ab90d986379aa63658f0300e")


main :: IO ()
main = siteComponent c >>= runAppReactive
