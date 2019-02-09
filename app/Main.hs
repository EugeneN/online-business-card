{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Lib            (SiteConfig(..), GistId(..), siteComponent)
import           Lubeck.App     (runAppReactive)


c = SiteConfig (GistId "ff3d182ce385cebb1774")


main :: IO ()
main = siteComponent c >>= runAppReactive
