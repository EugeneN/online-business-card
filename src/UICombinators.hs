{-# LANGUAGE OverloadedStrings          #-}

module UICombinators where

import           Data.JSString                  (JSString)
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A        
import qualified Web.VirtualDom.Html.Events     as E   

import           Lubeck.App                     (Html)


panel :: [Html] -> Html
panel = H.div [A.class_ "panel"]

blockPanel :: [Html] -> Html
blockPanel = H.div [A.class_ "blockPanel"]

cont :: [Html] -> Html
cont = H.div [A.class_ "contPanel"]

button :: JSString -> (E.Event -> IO ()) -> Html
button l h = H.button [A.class_ "button", E.click h] [H.text l]

label :: JSString -> Html
label t = H.span [A.class_ "label"] [H.text t]

vlayout :: Html -> Html -> Html -> Html -> Html
vlayout v1 v2 v3 v4 = 
    blockPanel 
        [ blockPanel [v1]
        , blockPanel [v2]
        , blockPanel [v3]
        , blockPanel [v4]
        ]

hlayout4 :: Html -> Html -> Html -> Html -> Html
hlayout4 v1 v2 v3 v4 = 
    cont 
        [ panel [v1]
        , panel [v2]
        , panel [v3]
        , panel [v4]
        ]

hlayout5 :: Html -> Html -> Html -> Html -> Html -> Html
hlayout5 v1 v2 v3 v4 v5 = 
    cont 
        [ panel [v1]
        , panel [v2]
        , panel [v3]
        , panel [v4]
        , panel [v5]
        ]

