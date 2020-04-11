{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid    ((<>))
import           Control.Monad  (when)
import           Lubeck.App     (runAppReactive)
import           Lubeck.FRP
import           Component.Site (siteComponent)
import           Lib
import           Web.VirtualDom


main :: IO ()
main = do
    z <- isLocalhost
    when (not z) redirectToHTTPS

    c <- readConfig
    case c of
        Left err -> runAppReactive . pureMsg $ "Error reading config: " <> err
        Right c' -> do
          x <- siteComponent c' 
          _ <- subscribeEvent (updates x) $ \x' -> renderToString x' >>= perfLog
          runAppReactive x
