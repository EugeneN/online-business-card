{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Lib            (SiteConfig(..), GistId(..), siteComponent)
import           Lubeck.App     (runAppReactive)


c = SiteConfig (GistId "8cbb546208d7a5c2fe978416c60ff97f")


main :: IO ()
main = siteComponent c >>= runAppReactive
