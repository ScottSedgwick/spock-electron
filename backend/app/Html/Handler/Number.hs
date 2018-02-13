module Html.Handler.Number where

import Html.Templates
import Html.Types

import Control.Monad (forM_)
import Data.Text (Text, unpack)
import Text.Blaze.XHtml5 (li, p, toHtml, ul)

hndNum :: Text -> MyApp () b
hndNum num = pageTemplate "Natural numbers" $ do
    p "A list of natural numbers"
    ul $ forM_ [1..n] (li . toHtml)
  where
    n = read (unpack num) :: Int