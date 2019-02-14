{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE JavaScriptFFI              #-}

module Component.Nav
    ( navComponent
    ) where

import           GHCJS.Types                    (JSString, JSVal)
import           GHCJS.Foreign.Callback
import qualified Data.JSString                  as JSS      
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (void)

import           Lubeck.FRP                     
import           Lubeck.Web.URI                 (decodeURIComponent)

import           Types
import           Lib


navComponent :: IO (Signal Path)
navComponent = do
  (u, s) <- newSignal [] :: FRP (Sink Path, Signal Path)

  onUrlHashChange =<< mkCallback (handleLocHash u)
  void . forkIO $ getUrlHash >>= handleLocHash' u

  pure s

  where
    handleLocHash u  = u . fmap decodeURIComponent . splitLocationHash . extractNewHash
    handleLocHash' u = u . fmap decodeURIComponent . splitLocationHash 

mkCallback :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
mkCallback f = syncCallback1 ThrowWouldBlock f

foreign import javascript unsafe "var x = $1; window.addEventListener('hashchange', x, false);"
  onUrlHashChange :: Callback (JSVal -> IO ()) -> IO ()

splitLocationHash :: JSString -> [JSString]
splitLocationHash h = if JSS.length h < 1 then [] else Prelude.filter (/= "") . JSS.splitOn "/" $ h

foreign import javascript unsafe "document.location.hash.replace('#', '')"
  getUrlHash :: IO JSString

foreign import javascript unsafe "(function(z){ return z.newURL.split('#')[1] || ''; }($1))"
  extractNewHash :: JSVal -> JSString 