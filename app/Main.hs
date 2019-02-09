{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Lib            (siteComponent)
import           Lubeck.App     (runAppReactive)

import Types


c = SiteConfig (GistId "2f741d60ab90d986379aa63658f0300e")


main :: IO ()
main = siteComponent c >>= runAppReactive
