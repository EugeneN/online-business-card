{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Lib            (SiteConfig(..), GistId(..), siteComponent)
import           Lubeck.App     (runAppReactive)


c = SiteConfig (GistId "b0bb1c06c091b06264f939748df0cf3a")


main :: IO ()
main = siteComponent c >>= runAppReactive
