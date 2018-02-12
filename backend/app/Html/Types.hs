module Html.Types where

import Data.IORef (IORef)
import Web.Spock (ActionCtxT, WebStateM)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)
type MyApp ctx a = ActionCtxT ctx (WebStateM () MySession MyAppState) a