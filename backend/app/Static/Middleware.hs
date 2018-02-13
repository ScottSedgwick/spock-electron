{-# LANGUAGE TemplateHaskell #-}
module Static.Middleware (staticResourceMiddleware) where

import Static.EmbeddedResources (mkEmbedded)

import Network.Wai (Middleware)
import Network.Wai.Application.Static (staticApp)
import WaiAppStatic.Storage.Embedded
import WaiAppStatic.Types (StaticSettings(..))

staticResourceMiddleware :: Middleware
staticResourceMiddleware nextApp = staticApp $ $(mkSettings mkEmbedded) { ss404Handler = Just nextApp }