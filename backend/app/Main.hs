{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (Main.main) where

----------------------------------------------------------------------------------------------
-- Import libraries
import           Control.Monad                  (forM_)
import           Control.Monad.Logger           (NoLoggingT, runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Trans            (MonadIO, liftIO)

import Control.Monad.Trans.Resource

import           Data.Monoid                    ((<>))
import           Data.IORef                     (IORef, atomicModifyIORef', newIORef)
import           Data.Text                      (Text, pack, unpack)
import           Database.Persist
import           Database.Persist.Sql           (SqlBackend)
import           Database.Persist.Sqlite        --(createSqlitePool, runMigration, runSqlPool)
-- import qualified Database.Persist.Sqlite        as DB
import           Database.Persist.TH
import           Network.Wai.Handler.Warp       (defaultSettings, runSettings, setPort, setHost)
import           System.Directory               (getCurrentDirectory)
import           Text.Blaze.XHtml5              (a, h1, li, p, toHtml, ul, (!))
import           Text.Blaze.XHtml5.Attributes   (class_, href)
import           Web.Spock                      (ActionCtxT, HasSpock, SpockConn, SpockCtxM, SpockState, WebStateM, (<//>), get, getState, root, runQuery, spock, spockAsApp, var)
import           Web.Spock.Config               (PoolOrConn(PCPool), defaultSpockCfg)

-- import           Control.Monad.IO.Class  (liftIO)
-- import           Database.Persist
-- import           Database.Persist.Sqlite
-- import           Database.Persist.TH
----------------------------------------------------------------------------------------------
-- Import local modules
import           Html.Templates                (pageTemplate, role)
import           Static.Middleware             (staticResourceMiddleware)

----------------------------------------------------------------------------------------------
-- Types
data MySession = EmptySession                -- Session data? Don't know how to use this yet
data MyAppState = DummyAppState (IORef Int)  -- Placeholder for application state
 
----------------------------------------------------------------------------------------------
-- Persistence layer
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Recipient
    firstName String
    deriving Show
Implant
    recipientId RecipientId
    implantType String
    deriving Show
|]

----------------------------------------------------------------------------------------------
-- Application setup
-- Using 127.0.0.1 as the host binding means the OS never asks if it is OK to spin up the server
-- TODO: Figure out how to find an unused port, rather than hard-code 8080.
-- TODO: Make a better place to store the DB file - e.g. ~/.appname/appname.db
main :: IO ()
main = do 
    let settings = setPort 8080 $ setHost "127.0.0.1" defaultSettings
    ref <- newIORef 0
    pool <- runStdoutLoggingT $ createSqlitePool "test.db" 5
    runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    spockCfg <- defaultSpockCfg EmptySession (PCPool pool) (DummyAppState ref)
    app <- spockAsApp (spock spockCfg myApp)
    runSettings settings $ staticResourceMiddleware app 

----------------------------------------------------------------------------------------------
-- Routing
myApp :: SpockCtxM ctx SqlBackend MySession MyAppState ()
myApp = do 
    Web.Spock.get root               hndRoot
    Web.Spock.get ("hello" <//> var) hndHello
    Web.Spock.get ("num"   <//> var) hndNum
    Web.Spock.get ("ins"   <//> var) hndInsRecip
    Web.Spock.get "list"             hndList
    
----------------------------------------------------------------------------------------------
-- Handlers
hndRoot :: ActionCtxT ctx (WebStateM SqlBackend MySession MyAppState) b
hndRoot = do
    cwd <- liftIO getCurrentDirectory
    pageTemplate "Navbar test" $ do
        h1 "Navbar Example"
        p $ toHtml cwd
        p ! class_ "lead" $ "This example is a quick exercise to illustrate how the top-aligned navbar works. As you scroll, this navbar remains in its original position and moves with the rest of the page."
        a ! class_ "btn btn-lg btn-primary" ! href "https://getbootstrap.com/docs/4.0/components/navbar/" ! role "button" $ "View navbar docs"

-- This is an example of a handler that modifies the application state.
hndHello :: (SpockState (ActionCtxT ctx m) ~ MyAppState, MonadIO m, HasSpock (ActionCtxT ctx m)) => Text -> ActionCtxT ctx m b
hndHello aName = do 
    (DummyAppState ref) <- getState
    visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
    pageTemplate "Hello" $ p $ "Hello " <> toHtml aName <> ", you are visitor number " <> toHtml (pack (show visitorNumber))

hndNum :: MonadIO m => Text -> ActionCtxT ctx m a
hndNum num = pageTemplate "Natural numbers" $ do
    p "A list of natural numbers"
    ul $ forM_ [1..n] (li . toHtml)
  where
    n = read (unpack num) :: Int

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn
{-# INLINE runSQL #-}

hndInsRecip :: (SpockConn (ActionCtxT ctx m) ~ SqlBackend, MonadIO m, HasSpock (ActionCtxT ctx m)) => Text -> ActionCtxT ctx m b
hndInsRecip aName = do
    recipId <- runSQL (insert $ Recipient (unpack aName)) 
    pageTemplate "Inserted Recipient" $ do
        p $ "Inserted" <> toHtml aName <> " with ID " <> toHtml (pack (show recipId))

hndList :: (SpockConn (ActionCtxT ctx m) ~ SqlBackend, MonadIO m, HasSpock (ActionCtxT ctx m)) => ActionCtxT ctx m b
hndList = do
    allRecips <- runSQL $ selectList [] [Desc RecipientFirstName]
    pageTemplate "Recipient List" $ do
        h1 "Recipients"
        ul $ forM_ allRecips $ \r -> li $ toHtml $ recipientFirstName (entityVal r)


-- main :: IO ()
-- main = runSqlite ":memory:" $ do
--     runMigration migrateAll

--     johnId <- insert $ Person "John Doe" $ Just 35
--     janeId <- insert $ Person "Jane Doe" Nothing

--     insert $ BlogPost "My fr1st p0st" johnId
--     insert $ BlogPost "One more for good measure" johnId

--     oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
--     liftIO $ print (oneJohnPost :: [Entity BlogPost])

--     john <- DB.get johnId
--     liftIO $ print (john :: Maybe Person)

--     delete janeId
--     deleteWhere [BlogPostAuthorId ==. johnId]