{-# LANGUAGE OverloadedStrings #-}
module Html.Templates where

import Html.Attributes
import Html.Types
import Static.Resources(cssResources, jsResources)

import Control.Monad (forM_)
import Prelude hiding (head, id, div, span)
import Text.Blaze (customAttribute, dataAttribute, textValue, unsafeByteStringValue)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.XHtml5 (Html, a, body, button, div, docTypeHtml, form, head, input, li, link, main, meta, nav, script, span, title, toHtml, ul, (!))
import Text.Blaze.XHtml5.Attributes (charset, class_, content, href, id, lang, name, placeholder, rel, src, type_)
import Web.Spock (lazyBytes)

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
    forM_ cssResources (\(path,mime,_) -> link ! rel "stylesheet" ! type_ (unsafeByteStringValue mime) ! href (textValue path))

bodyTemplate :: Html -> Html
bodyTemplate aBody = body $ do
    navBar
    main ! role "main" ! class_ "container" $ 
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