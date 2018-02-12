{-# LANGUAGE OverloadedStrings #-}
module Main where

import Html.Handler.Hello       (hndHello)
import Html.Handler.Number      (hndNum)
import Html.Handler.Root        (hndRoot)
import Html.Types
import Static.Middleware        (staticResourceMiddleware)

import Data.IORef               (newIORef)
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Web.Spock                (SpockM, get, root, spockAsApp, spock, var, (<//>))
import Web.Spock.Config         (PoolOrConn(..), defaultSpockCfg)

main :: IO ()
main = do 
    mySpockApp <- mySpockAppIO
    run 8080 $ staticResourceMiddleware mySpockApp

mySpockAppIO :: IO Application
mySpockAppIO = do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    spockAsApp $ spock spockCfg myApplication

myApplication :: SpockM () MySession MyAppState ()
myApplication = do
    get root               hndRoot
    get ("hello" <//> var) hndHello
    get ("num" <//> var)   hndNum
