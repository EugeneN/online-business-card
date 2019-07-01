{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI     #-}

module Lib 
 ( htmlStringToVirtualDom
 , newSignal
 , extractMenu
 , isLocalhost
 , redirectToHTTPS
 , btoa
 , readConfig
 , writeConfig
 , pureMsg
 , renderPath
 , writeBlogIndex
 , redirectLocal
 , isBlog
 , isSpecialPath
 , perfLog
 ) where

import           Data.Aeson
import qualified Data.ByteString.Char8          as BS
import           Data.JSString                  (JSString)
import qualified Data.JSString                  as JSS  
import           Data.Maybe                     (listToMaybe)
import           Data.Monoid ((<>))
import qualified Data.Tree                      as DT

import qualified Text.XML.Light.Input           as XMLI
import qualified Text.XML.Light.Types           as XMLT

import qualified Web.VirtualDom.Html            as H
import qualified Web.VirtualDom.Html.Attributes as A  
import qualified Web.VirtualDom                 as VirtualDom

import qualified JavaScript.Web.Location       as WL

import           Lubeck.App                     (Html)
import           Lubeck.FRP   
import           Lubeck.Util   

import           Types

pureMsg :: JSString -> Signal Html
pureMsg x = pure $ H.div [A.class_ "pure-msg"] [H.text x] 

renderPath :: Path -> JSString
renderPath ps = "#" <> JSS.intercalate "/" ps

newSignal :: a -> FRP (Sink a, Signal a)
newSignal z = do
  (u, e) <- newEvent
  s <- stepperS z e
  pure (u, s)

extractMenu :: DT.Forest Page -> Path -> Path -> Menu
extractMenu f []      bc = Menu (MenuLevel $ fmap (treeToMenuItem bc) f) MenuNil
extractMenu f (p0:ps) bc = 
  let curLevel = MenuLevel . fmap (pageToMenuItem p0 bc) . fmap DT.rootLabel $ f
      curTree  = listToMaybe $ Prelude.filter ((p0 ==) . path . DT.rootLabel) f
      subLevel = case curTree of
                    Nothing -> MenuNil
                    Just t  -> extractMenu (DT.subForest t) ps (bc <> [path $ DT.rootLabel t])
  in Menu curLevel subLevel

treeToMenuItem :: Path -> DT.Tree Page -> MenuItem
treeToMenuItem bc t = let p = DT.rootLabel t in MIUnselected (title p) (bc <> [path p]) (isSpecial p)

pageToMenuItem :: Url -> Path -> Page -> MenuItem
pageToMenuItem p0 bc p = 
  if path p == p0 
    then MISelected   (title p) (bc <> [path p]) (isSpecial p) 
    else MIUnselected (title p) (bc <> [path p]) (isSpecial p) 

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
            , "details" , "summary", "blockquote", "embed", "iframe"
            , "style", "button", "script", "input"]

validAttrs :: [Attr]
validAttrs = [ "class", "id" , "href" , "src" , "alt" , "title" , "style" , "lang" , "name" 
             , "target" , "width" , "height" , "min" , "max", "pluginspage"
             , "data-subscribe", "data-extend", "type", "colspan"]

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

    scriptContent :: [XMLT.Content] -> JSString
    scriptContent xs = JSS.unlines $ fmap c2s xs

    c2s :: XMLT.Content -> JSString
    c2s (XMLT.Elem z)                  = showJS z
    c2s (XMLT.Text (XMLT.CData _ z _)) = JSS.pack z
    c2s (XMLT.CRef _)                  = ""

    go (XMLT.Text (XMLT.CData _ x _))                                   = H.text $ JSS.pack x
    go (XMLT.Elem (XMLT.Element (XMLT.QName tag _ _) attrs children _)) = if isValidTag (JSS.pack tag)
                                                                            then if (JSS.pack tag) == "script"
                                                                                --  then VirtualDom.node (JSS.pack tag) (fmap goAttrs attrs) [H.text $ scriptContent children]
                                                                                 then VirtualDom.node "script" (fmap goAttrs attrs) [H.text $ scriptContent children]
                                                                                 else VirtualDom.node (JSS.pack tag) (fmap goAttrs attrs) (fmap go children)
                                                                            else H.text $ "<Invalid tag: " <> JSS.pack tag <> ">"
    go (XMLT.CRef _)                                                    = H.text " " -- $ JSS.pack x

    goAttrs (XMLT.Attr (XMLT.QName key _ _) val) = if isValidAttrName (JSS.pack key) && isValidAttrVal (JSS.pack val)
                                                     then VirtualDom.attribute (JSS.pack key) (JSS.pack val)
                                                     else VirtualDom.attribute ("invalid-attr:" <> JSS.pack key) ""


isBlog :: Url -> Path -> Bool
isBlog _  []    = False
isBlog bs (x:_) = x == bs

isSpecialPath :: Path -> [Url] -> Bool
isSpecialPath []     _   = False
isSpecialPath (x:[]) _   = False
isSpecialPath (x:_)  cms = x `elem` cms

redirectLocal :: Path -> IO ()
redirectLocal = redirectLocal_ . renderPath

foreign import javascript unsafe "document.location.hash = $1" redirectLocal_ :: JSString -> IO ()

foreign import javascript unsafe "btoa($1)" btoa :: JSString -> JSString

foreign import javascript unsafe "if (location.protocol == 'http:') { location.href = 'https:' + window.location.href.substring(window.location.protocol.length); }"
  redirectToHTTPS :: IO ()

foreign import javascript unsafe "window.RootG ? window.RootG.toString() : '' "
  getConfig :: IO JSString

foreign import javascript unsafe "window._perfLog=window._perfLog || []; var x=[performance.now(), $1]; window._perfLog.push(x);"
  perfLog :: JSString -> IO ()

readConfig :: IO (Either JSString SiteConfig)
readConfig = do
  c <- getConfig 
  pure $ case eitherDecodeStrict' . BS.pack . JSS.unpack $ c of
    Left x   -> Left $ showJS x
    Right c' -> Right c'

writeConfig :: SiteConfig -> IO ()
writeConfig = print . encode

writeBlogIndex :: BlogIndex -> IO ()
writeBlogIndex = print . encode

isLocalhost :: IO Bool
isLocalhost = do
  loc  <- WL.getWindowLocation
  host <- WL.getHostname loc
  pure $ host == "localhost" || host == "127.0.0.1" || host == ""  -- XXX consider CIDR 127.