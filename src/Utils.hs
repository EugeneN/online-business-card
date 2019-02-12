{-# LANGUAGE OverloadedStrings #-}

module Utils 
 ( htmlStringToVirtualDom
 , jss2text
 , text2jss
 , newSignal
 ) where

import           Data.JSString                  (JSString)
import qualified Data.JSString                  as JSS  
import           Data.Monoid ((<>))
import qualified Data.Text                      as T

import qualified Text.XML.Light.Input           as XMLI
import qualified Text.XML.Light.Types           as XMLT

import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom                 as VirtualDom

import           Lubeck.App                     (Html)
import           Lubeck.FRP   

jss2text :: JSString -> T.Text
jss2text = T.pack . JSS.unpack

text2jss :: T.Text -> JSString 
text2jss = JSS.pack . T.unpack

newSignal :: a -> FRP (Sink a, Signal a)
newSignal z = do
  (u, e) <- newEvent
  s <- stepperS z e
  pure (u, s)

--------------------------------------------------------------------------------
type Tag = JSString
type Attr = JSString

validTags :: [Tag]
validTags = [ "address" , "article" , "body" , "footer" , "header" , "h1" , "h2" , "h3"
            , "h4" , "h5" , "h6" , "nav" , "section" , "dd" , "div" , "dl" , "dt" , "figcaption"
            , "figure" , "hr" , "li" , "ol" , "p" , "pre" , "ul" , "a" , "abbr" , "b"
            , "br" , "cite" , "code" , "dfn" , "em" , "i" , "kbd" , "mark" , "q" , "rp"
            , "rt" , "s" , "samp" , "small" , "span" , "strong" , "sub" , "sup"
            , "time" , "u" , "var" , "wbr" , "img" , "video"
            , "source" , "del" , "ins" , "caption"
            , "col" , "colgroup" , "table" , "tbody" , "td" , "tfoot" , "th" , "thead"
            , "tr" , "datalist" , "fieldset"
            , "label" , "legend" , "meter" , "optgroup" , "option"
            , "details" , "summary", "blockquote", "embed", "iframe"]

validAttrs :: [Attr]
validAttrs = [ "class", "id" , "href" , "src" , "alt" , "title" , "style" , "lang" , "name" , "target" , "width" , "height" , "min" , "max", "pluginspage"]

isValidTag :: Tag -> Bool
isValidTag = (`elem` validTags)

isValidAttrName :: Attr -> Bool
isValidAttrName = (`elem` validAttrs)

-- TODO validate attr content, to prevent dynamic, scripting content etc
-- https://www.owasp.org/index.php/XSS_(Cross_Site_Scripting)_Prevention_Cheat_Sheet
-- to prevent things like these:
-- { background-url : "javascript:alert(1)"; }  // and all other URLs
-- { text-size: "expression(alert('XSS'))"; }   // only in IE

-- TODO unicode-entities-encoded values
isValidAttrVal :: JSString -> Bool
isValidAttrVal x =
  let a = JSS.count "javascript" . JSS.toLower $ x
      b = JSS.count "expression" . JSS.toLower $ x
      c = JSS.count "file" . JSS.toLower $ x
  in a + b + c == 0

htmlStringToVirtualDom :: JSString -> [Html]
htmlStringToVirtualDom s = fmap go htmlAST
  where
    htmlAST = XMLI.parseXML $ JSS.unpack s

    go (XMLT.Text (XMLT.CData _ x _))                                   = H.text $ JSS.pack x
    go (XMLT.Elem (XMLT.Element (XMLT.QName tag _ _) attrs children _)) = if isValidTag (JSS.pack tag)
                                                                            then VirtualDom.node (JSS.pack tag) (fmap goAttrs attrs) (fmap go children)
                                                                            else H.text $ "<Invalid tag: " <> JSS.pack tag <> ">"
    go (XMLT.CRef _)                                                    = H.text " " -- $ JSS.pack x

    goAttrs (XMLT.Attr (XMLT.QName key _ _) val) = if isValidAttrName (JSS.pack key) && isValidAttrVal (JSS.pack val)
                                                     then VirtualDom.attribute (JSS.pack key) (JSS.pack val)
                                                     else VirtualDom.attribute ("invalid-attr:" <> JSS.pack key) ""


--------------------------------------------------------------------------------    