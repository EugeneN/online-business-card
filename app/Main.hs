{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid    ((<>))
import           Lubeck.App     (runAppReactive)
import           Component.Site (siteComponent)
import           Lib


main :: IO ()
main = do
    z <- isLocalhost
    if not z then redirectToHTTPS else pure ()

    c <- readConfig
    case c of
        Left err -> runAppReactive . pureMsg $ "Error reading config: " <> err
        Right c' -> siteComponent c' >>= runAppReactive
