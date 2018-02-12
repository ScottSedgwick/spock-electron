{-# LANGUAGE OverloadedStrings #-}
module Html.Handler.Root where

import Html.Attributes
import Html.Templates
import Html.Types

import Control.Monad.Trans (liftIO)
import System.Directory (getCurrentDirectory)
import Text.Blaze.XHtml5 (a, h1, p, toHtml, (!))
import Text.Blaze.XHtml5.Attributes (class_, href)

hndRoot :: MyApp () a
hndRoot = do
    cwd <- liftIO getCurrentDirectory
    pageTemplate "Navbar test" $ do
        h1 "Navbar Example"
        p $ toHtml cwd
        p ! class_ "lead" $ "This example is a quick exercise to illustrate how the top-aligned navbar works. As you scroll, this navbar remains in its original position and moves with the rest of the page."
        a ! class_ "btn btn-lg btn-primary" ! href "https://getbootstrap.com/docs/4.0/components/navbar/" ! role "button" $ "View navbar docs"