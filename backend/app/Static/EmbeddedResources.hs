module Static.EmbeddedResources (mkEmbedded) where

import WaiAppStatic.Storage.Embedded
import Crypto.Hash.MD5 (hashlazy)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Static.Resources (ResourceInfo, resourceItems)

hash :: BL.ByteString -> T.Text
hash = T.take 8 . T.decodeUtf8 . B64.encode . hashlazy

mkEntry :: ResourceInfo -> IO EmbeddableEntry
mkEntry (locn, mimetype, filepath) = do
    filedata <- BL.readFile filepath
    pure EmbeddableEntry {
              eLocation = locn
            , eMimeType = mimetype
            , eContent = Left (hash filedata, filedata)
            }

mkEmbedded :: IO [EmbeddableEntry]
mkEmbedded = sequence $ mkEntry <$> resourceItems