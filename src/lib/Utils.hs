module Utils where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL8
import Prelude

stringFromJSON :: (ToJSON a) => a -> String
stringFromJSON = init . tail . BL8.unpack . Aeson.encode


