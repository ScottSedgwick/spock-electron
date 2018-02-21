{-# LANGUAGE TemplateHaskell #-}
{-
    The modules in the app/Static folder are an example of how to statically compile static 
    resources (js, css, images) into the web server executable.
    The alternative is to have a directory loader, but to do that you need to either know the path
    to the static resources directory, or load that path from configuration at run time. While this
    is sensible for a real web server, it is not practical for the back end to an electron 
    application.
-}
module Static.Middleware (staticResourceMiddleware) where

import Static.EmbeddedResources (mkEmbedded)

import Network.Wai (Middleware)
import Network.Wai.Application.Static (staticApp)
import WaiAppStatic.Storage.Embedded
import WaiAppStatic.Types (StaticSettings(..))

----------------------------------------------------------------------------------------------
-- This is a Middleware (:: Application -> Application) that serves static items if found, or
-- passes the request on to the "nextApp".
-- The slightly funky syntax "$(mkSettings mkEmbedded)" is template Haskell, and it runs 
-- some Haskell code at compile time to generate the statically compiled resources.
staticResourceMiddleware :: Middleware
staticResourceMiddleware nextApp = staticApp $ $(mkSettings mkEmbedded) { ss404Handler = Just nextApp }