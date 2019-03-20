{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid    ((<>))
import           Lubeck.App     (runAppReactive)
import           Lubeck.FRP
import           Lubeck.Util
import           Component.Site (siteComponent)
import           Lib
import           Web.VirtualDom


main :: IO ()
main = do
    z <- isLocalhost
    if not z then redirectToHTTPS else pure ()

    c <- readConfig
    case c of
        Left err -> runAppReactive . pureMsg $ "Error reading config: " <> err
        Right c' -> do
          x <- siteComponent c' 
          subscribeEvent (updates x) $ \x -> renderToString x >>= perfLog
          runAppReactive x
