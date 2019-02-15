{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lubeck.App     (runAppReactive)
import           Component.Site (siteComponent)
import           Types
import           Lib


c :: SiteConfig
c = SiteConfig (GistId "2f741d60ab90d986379aa63658f0300e")

main :: IO ()
main = do
    z <- isLocalhost
    if not z then redirectToHTTPS else pure ()
    siteComponent c >>= runAppReactive
