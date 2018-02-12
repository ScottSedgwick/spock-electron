{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text, pack, unpack)

import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static (staticApp)
import WaiAppStatic.Types (StaticSettings(..))
import WaiAppStatic.Storage.Embedded

import Prelude hiding (head, id, div, span)
import System.Directory (getCurrentDirectory)

import Text.Blaze (customAttribute, dataAttribute, textValue)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.XHtml5 (Html, a, body, button, div, docTypeHtml, form, h1, head, input, li, link, meta, nav, p, script, span, title, toHtml, ul, (!))
import qualified Text.Blaze.XHtml5 as B (main)
import Text.Blaze.XHtml5.Attributes (charset, class_, content, href, id, lang, name, placeholder, rel, src, type_)

import Web.Spock (ActionCtxT, SpockM, WebStateM, get, getState, lazyBytes, root, spockAsApp, spock, var, (<//>))
import Web.Spock.Config (PoolOrConn(..), defaultSpockCfg)

import Html.Attributes
import Static.EmbeddedResources (mkEmbedded)
import Static.Resources(cssResources, jsResources)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)
type MyApp ctx a = ActionCtxT ctx (WebStateM () MySession MyAppState) a

staticResourceMiddleware :: Middleware
staticResourceMiddleware nextApp = staticApp $ $(mkSettings mkEmbedded) { ss404Handler = Just nextApp }

spockApplication :: IO Application
spockApplication = do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    spockAsApp $ spock spockCfg myApplication

main :: IO ()
main = do 
    myApp <- spockApplication
    run 8080 $ staticResourceMiddleware myApp

myApplication :: SpockM () MySession MyAppState ()
myApplication = do
    get root               hdnRoot
    get ("hello" <//> var) hndHello
    get ("num" <//> var)   hndNum

hndHello :: Text -> MyApp () b
hndHello aName = do 
    (DummyAppState ref) <- getState
    visitorNumber <- liftIO $ atomicModifyIORef' ref $ \x -> (x+1, x+1)
    pageTemplate "Hello" $ p $ "Hello " <> toHtml aName <> ", you are visitor number " <> toHtml (pack (show visitorNumber))

hdnRoot :: MyApp () a
hdnRoot = do
    cwd <- liftIO getCurrentDirectory
    pageTemplate "Navbar test" $ do
        h1 "Navbar Example"
        p $ toHtml cwd
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
    forM_ cssResources (\(path,_,_) -> link ! rel "stylesheet" ! type_ "text/css" ! href (textValue path))

bodyTemplate :: Html -> Html
bodyTemplate aBody = body $ do
    navBar
    B.main ! role "main" ! class_ "container" $ 
        div ! class_ "jumbotron" $ 
            aBody
    forM_ jsResources (\(path,_,_) -> script ("" :: Html) ! src (textValue path))

navBar :: Html
navBar = 
    nav ! class_ "navbar navbar-expand-md navbar-dark bg-dark mb-4" $ do
        a ! class_ "navbar-brand" ! href "#" $ "Top navbar"
        button ! class_ "navbar-toggler" 
               ! type_ "button" 
               ! dataAttribute "toggle" "collapse" 
               ! dataAttribute "target" "#navbarCollapse" 
               ! customAttribute "aria-controls" "navbarCollapse"
               ! customAttribute "aria-expanded" "false" 
               ! customAttribute "aria-label" "Toggle navigation" $ 
            span ! class_ "navbar-toggler-icon" $ ""
        div ! class_ "collapse navbar-collapse" ! id "navbarCollapse" $ do
            ul ! class_ "navbar-nav mr-auto" $ do
                li ! class_ "nav-item active" $ 
                    a ! class_ "nav-link" ! href "#" $ "Home"
                li ! class_ "nav-item" $ 
                    a ! class_ "nav-link" ! href "#" $ "Link"
                li ! class_ "nav-item" ! href "#" $ 
                    a ! class_ "nav-link disabled" ! href "#" $ "Disabled"
            form ! class_ "form-inline mt-2 mt-md-0" $ do
                input ! class_ "form-control mr-sm-2" ! type_ "text" ! placeholder "Search" ! customAttribute "aria-label" "Search"
                button ! class_ "btn btn-outline-success my-2 my-sm-0" $ "Search"