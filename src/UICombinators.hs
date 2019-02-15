{-# LANGUAGE OverloadedStrings          #-}

module UICombinators where

import           Data.JSString                  (JSString)
import           Data.Monoid                    ((<>))
import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A        
import qualified Web.VirtualDom.Html.Events     as E   
import qualified Web.VirtualDom                 as VirtualDom

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

passwordWidget :: Bool -> Widget' JSString
passwordWidget focus sink value =
  H.input
    ([ A.type_ "password"
    -- TODO size
    , A.class_ "form-control"
    , A.value value
    , E.change  $ contramapSink E.value sink
    , E.keyup $ contramapSink E.value sink
    ] <> fcs) []
  where
    fcs = [A.autofocus True | focus]

richEditorWidget :: Bool -> Widget' JSString
richEditorWidget focus sink value =
  H.div [A.class_ "noui"]
        [ H.textarea
            ([ A.class_ "form-control"
              , E.change handler
              , E.keyup handler -- will not work as element gets replaced
              ] <> fcs) [H.text value]
        , VirtualDom.node "script" [] [H.text editorScript]
        ]
  where
    fcs = [A.autofocus True | focus]

    handler ev = sink $ E.value ev

    editorScript = "(function() { \
                    \  var me = document.currentScript;\
                    \  var taElem = me.previousSibling;\
                    \  setTimeout( function () {\
                    \    var editor = CKEDITOR.replace( taElem );\
                    \    console.log('init ckeditor');\
                    \    editor.on('change', function(evt){\
                    \        editor.updateElement();\
                    \        var event = new Event('change');\
                    \        taElem.dispatchEvent(event);\
                    \    });\
                    \  }, 10);\
                    \}())"