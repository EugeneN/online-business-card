{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid    ((<>))
import           Control.Monad  (when)
import           Lubeck.App     (runAppReactive)
import           Lubeck.FRP
import           Component.Site (siteComponent)
import           Lib
import           Types
import           Web.VirtualDom


main :: IO ()
main = do
    -- z <- isLocalhost
    -- when (not z) redirectToHTTPS

    -- c <- readConfig
    let c = Right $ SiteConfig (GistId "2f741d60ab90d986379aa63658f0300e") 
                               (GistId "72aba9a613d411a528ac2bc4b780538b")
    case c of
        Left err -> runAppReactive . pureMsg $ "Error reading config: " <> err
        Right c' -> do
          x <- siteComponent c' 
          _ <- subscribeEvent (updates x) $ \x' -> renderToString x' >>= perfLog
          runAppReactive x
