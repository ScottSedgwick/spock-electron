{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text, pack, unpack)

import Network.Wai.Middleware.Static (addBase, staticPolicy)

import Prelude hiding (head, id, div, span)

import Text.Blaze (AttributeValue, customAttribute, dataAttribute)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.XHtml5 (Html, a, body, button, div, docTypeHtml, form, h1, head, input, li, link, meta, nav, p, script, span, title, toHtml, ul, (!))
import qualified Text.Blaze.XHtml5 as B (main)
import Text.Blaze.XHtml5.Attributes (charset, class_, content, href, id, lang, name, placeholder, rel, src, type_)

import Web.Spock (ActionCtxT, SpockM, WebStateM, get, getState, lazyBytes, middleware, root, runSpock, spock, var, (<//>))
import Web.Spock.Config (PoolOrConn(..), defaultSpockCfg)

import Html.Attributes

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)
type MyApp ctx a = ActionCtxT ctx (WebStateM () MySession MyAppState) a

main :: IO ()
main = do 
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
    middleware (staticPolicy (addBase "static")) 
    get root               hdnRoot
    get ("hello" <//> var) hndHello
    get ("num" <//> var)   hndNum

hndHello :: Text -> MyApp () b
hndHello aName = do 
    (DummyAppState ref) <- getState
    visitorNumber <- liftIO $ atomicModifyIORef' ref $ \x -> (x+1, x+1)
    pageTemplate "Hello" $ p $ "Hello " <> (toHtml aName) <> ", you are visitor number " <> toHtml (pack (show visitorNumber))

hdnRoot :: MyApp () a
hdnRoot = pageTemplate "Navbar test" $ do
    h1 "Navbar Example"
    p ! class_ "lead" $ "This example is a quick exercise to illustrate how the top-aligned navbar works. As you scroll, this navbar remains in its original position and moves with the rest of the page."
    a ! class_ "btn btn-lg btn-primary" ! href "https://getbootstrap.com/docs/4.0/components/navbar/" ! role "button" $ "View navbar docs"

hndNum :: Text -> MyApp () b
hndNum num = pageTemplate "Natural numbers" $ do
    p "A list of natural numbers"
    ul $ forM_ [1..n] (li . toHtml)
  where
    n = read (unpack num) :: Int

pageTemplate :: String -> Html -> MyApp () a
pageTemplate aTitle aBody = lazyBytes $ renderHtml $ docTypeHtml ! lang "en" $ do
    headerTemplate aTitle
    bodyTemplate aBody

headerTemplate :: String -> Html
headerTemplate aTitle = head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1, shrink-to-fit=no"
    meta ! name "description" ! content ""
    meta ! name "author" ! content ""
    link ! rel "Icon" ! href "/img/favicon.ico"
    title $ toHtml aTitle
    link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap.min.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "/css/navbar-top.css" 

bodyTemplate :: Html -> Html
bodyTemplate aBody = body $ do
    navBar
    B.main ! role "main" ! class_ "container" $ do
        div ! class_ "jumbotron" $ do
            aBody
    mkScript "/js/jquery-3.2.1.slim.min.js"
    script "window.jQuery || document.write('<script src=\"/js/jquery-slim.min.js\"><\\/script>')"
    mkScript "/js/popper.min.js"
    mkScript "/js/bootstrap.min.js"

mkScript :: AttributeValue -> Html
mkScript x = script ("" :: Html) ! src x

navBar :: Html
navBar = do
    nav ! class_ "navbar navbar-expand-md navbar-dark bg-dark mb-4" $ do
        a ! class_ "navbar-brand" ! href "#" $ "Top navbar"
        button ! class_ "navbar-toggler" 
               ! type_ "button" 
               ! dataAttribute "toggle" "collapse" 
               ! dataAttribute "target" "#navbarCollapse" 
               ! customAttribute "aria-controls" "navbarCollapse"
               ! customAttribute "aria-expanded" "false" 
               ! customAttribute "aria-label" "Toggle navigation" $ do
            span ! class_ "navbar-toggler-icon" $ ""
        div ! class_ "collapse navbar-collapse" ! id "navbarCollapse" $ do
            ul ! class_ "navbar-nav mr-auto" $ do
                li ! class_ "nav-item active" $ do
                    a ! class_ "nav-link" ! href "#" $ "Home"
                li ! class_ "nav-item" $ do
                    a ! class_ "nav-link" ! href "#" $ "Link"
                li ! class_ "nav-item" ! href "#" $ do
                    a ! class_ "nav-link disabled" ! href "#" $ "Disabled"
            form ! class_ "form-inline mt-2 mt-md-0" $ do
                input ! class_ "form-control mr-sm-2" ! type_ "text" ! placeholder "Search" ! customAttribute "aria-label" "Search"
                button ! class_ "btn btn-outline-success my-2 my-sm-0" $ "Search"