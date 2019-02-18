{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Component.Notification
  ( notificationsComponent
  , Notification(..)
  , nerr
  , nwarn
  , ninfo
  , nsuccess
  , welcome
  ) where

import           Prelude                        

import           GHCJS.Types                    (JSString)
import           Data.Monoid                    ((<>))

import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events     as E 

import           Lubeck.App                     (Html)
import           Lubeck.Forms
import           Lubeck.FRP
import           Lubeck.Util


type FgColor = JSString
type BgColor = JSString
type Message = JSString

data Notification = Notification FgColor BgColor Message

nerr :: JSString -> Notification
nerr = Notification "white" "red"

nwarn :: JSString -> Notification
nwarn = Notification "white" "orange"

ninfo :: JSString -> Notification
ninfo = Notification "white" "blue"

nsuccess :: JSString -> Notification
nsuccess = Notification "white" "green"

welcome :: JSString -> Notification
welcome = Notification "white" "pink"

notificationsComponent :: [Notification] -> FRP (Signal Html, Sink (Maybe Notification))
notificationsComponent initialErrorMessages = do
  (internalSink, internalEvents) <- newSyncEventOf (undefined :: Int)
  (externalSink, externalEvents) <- newSyncEventOf (undefined :: Maybe Notification)

  let inputE    = fmap externalToInternal externalEvents :: Events ([Notification] -> [Notification])
  let filterE   = fmap filterByIdx internalEvents        :: Events ([Notification] -> [Notification])
  let allEvents = merge inputE filterE                   :: Events ([Notification] -> [Notification])

  errorsS   <- accumS initialErrorMessages allEvents :: IO (Signal [Notification])
  let htmlS = fmap (notificationW internalSink) errorsS

  return (htmlS, externalSink)

  where
    externalToInternal :: Maybe a -> [a] -> [a]
    externalToInternal Nothing oldAs = oldAs
    externalToInternal (Just a) oldAs = oldAs <> [a]

    filterByIdx :: Int -> [a] -> [a]
    filterByIdx idxToRemove oldAs =
      fst <$> Prelude.filter ((/= idxToRemove) . snd) (zip oldAs [0..])

    notificationW :: Widget [Notification] Int
    notificationW _    [] = H.div [A.class_ "notifications-panel empty"] []
    notificationW sink ns = H.div [A.class_ "notifications-panel"]       [ H.div [] (fmap (notifItem sink) (zip [0..] ns))]
    
    notifItem sink (idx, Notification fg bg msg) = 
      H.div [ A.class_ $ "a-notification", A.style $ "color:" <> fg <> ";background-color:"<>bg<>";"]
            [ H.span   [A.class_ "a-msg"] [H.text msg]
            , H.button [A.class_ "close", A.style ("color:" <> fg), E.click $ \_ -> sink idx] [H.text "×"] ]
