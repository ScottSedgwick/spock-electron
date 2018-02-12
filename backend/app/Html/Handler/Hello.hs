{-# LANGUAGE OverloadedStrings #-}
module Html.Handler.Hello where

import Html.Templates
import Html.Types

import Control.Monad.Trans (liftIO)
import Data.IORef (atomicModifyIORef')
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Text.Blaze.XHtml5 (p, toHtml)
import Web.Spock (getState)

hndHello :: Text -> MyApp () b
hndHello aName = do 
    (DummyAppState ref) <- getState
    visitorNumber <- liftIO $ atomicModifyIORef' ref $ \x -> (x+1, x+1)
    pageTemplate "Hello" $ p $ "Hello " <> toHtml aName <> ", you are visitor number " <> toHtml (pack (show visitorNumber))