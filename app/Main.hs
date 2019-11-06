{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Main where

import           Data.Monoid    ((<>))
-- import           Lubeck.App     (runAppReactive)
import           Lubeck.FRP
import           Component.Site (siteComponent)
import           Lib
import           Web.VirtualDom

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Web.VirtualDom as VD
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (catch, SomeException)
import Lubeck.Html (Html)
import GHCJS.Foreign.Callback as CB


foreign import javascript unsafe
  "var loop = function() { $1(); requestAnimationFrame(loop) } ; loop()"
  reqAnimationFrameLoop :: Callback (IO ()) -> IO ()

runAppReactive :: Signal Html -> IO ()
runAppReactive s = flip catch (\e -> print (e :: SomeException)) $ do
  initVD <- pollBehavior $ current s
  initRD <- VD.createElement initVD

  prevVD <- newIORef initVD
  newVD  <- newIORef initVD
  varRD  <- newIORef initRD
  dirty  <- newIORef False

  VD.appendToBody initRD

  _ <- subscribeEvent (updates s) $ \newVD_ -> do
    writeIORef newVD newVD_
    writeIORef dirty True

  let processFrame prevVD newVD varRD = do
        isDirty <- readIORef dirty

        if isDirty
        then do
          print "doing anim"
          prevVD_ <- readIORef prevVD
          newVD_ <- readIORef newVD

          delta <- VD.diff prevVD_ newVD_
          
          prevRD <- readIORef varRD
          newRD <- VD.patch prevRD delta

          writeIORef prevVD newVD_
          writeIORef varRD newRD
          writeIORef dirty False
          return ()
        else
          return ()

  processFrameCB <- CB.asyncCallback (processFrame prevVD newVD varRD)
  reqAnimationFrameLoop processFrameCB
    
  return ()


main :: IO ()
main = do
    z <- isLocalhost
    if not z then redirectToHTTPS else pure ()

    c <- readConfig
    case c of
        Left err -> runAppReactive . pureMsg $ "Error reading config: " <> err
        Right c' -> do
          x <- siteComponent c' 
          _ <- subscribeEvent (updates x) $ \x' -> renderToString x' >>= perfLog
          runAppReactive x
