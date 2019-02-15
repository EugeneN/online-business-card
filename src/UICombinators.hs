{-# LANGUAGE OverloadedStrings          #-}

module UICombinators where

import           Data.JSString                  (JSString)
import           Data.Monoid                    ((<>))
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A        
import qualified Web.VirtualDom.Html.Events     as E   

-- import           Lubeck.App                     (Html)
import           Lubeck.FRP
import           Lubeck.Forms

stringWidget :: Bool -> Widget' JSString
stringWidget focus sink value =
  H.input
    ([ A.type_ "text"
    -- TODO size
    , A.class_ "form-control"
    , A.value value
    , E.change  $ contramapSink E.value sink
    , E.keyup $ contramapSink E.value sink
    ] <> fcs) []
  where
    fcs = [A.autofocus True | focus]

