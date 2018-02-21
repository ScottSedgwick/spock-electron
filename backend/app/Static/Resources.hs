module Static.Resources (ResourceInfo, cssResources, jsResources, resourceItems) where

import qualified Data.ByteString as B
import Data.Monoid ((<>))
import qualified Data.Text as T

----------------------------------------------------------------------------------------------
-- The ResourceInfo type should hold:
--   (URI path to resource, MIME type, path to static file in repository)
type ResourceInfo = (T.Text, B.ByteString, FilePath)

----------------------------------------------------------------------------------------------
-- Constants for MIME types (to avoid typos)
mimeCss :: B.ByteString
mimeCss = "text/css"

mimeJs :: B.ByteString
mimeJs = "application/javascript"

----------------------------------------------------------------------------------------------
-- List of CSS resources
cssResources :: [ResourceInfo]
cssResources =  [ ("css/bootstrap.min.css", mimeCss, "static/css/bootstrap.min.css")
                , ("css/navbar-top.css"   , mimeCss, "static/css/navbar-top.css") 
                ]

----------------------------------------------------------------------------------------------
-- List of Javascript resources
jsResources :: [ResourceInfo]
jsResources =   [ ("js/bootstrap.min.js"        , mimeJs, "static/js/bootstrap.min.js")
                , ("js/popper.min.js"           , mimeJs, "static/js/popper.min.js")
                , ("js/jquery-slim.min.js"      , mimeJs, "static/js/jquery-slim.min.js")
                , ("js/jquery-3.2.1.slim.min.js", mimeJs, "static/js/jquery-3.2.1.slim.min.js")
                ]

----------------------------------------------------------------------------------------------
-- Combined list of all resources (must be complete for Haskell compiler to pick everything up)
resourceItems :: [ResourceInfo]
resourceItems = cssResources <> jsResources