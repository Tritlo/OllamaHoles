-- | The Backend data type
module GHC.Plugin.OllamaHoles.Backend where

import Data.Text (Text)

-- | Backend to use.
data Backend = Backend
    { listModels :: IO (Maybe [Text])
    , generateFits :: Text -> Text -> IO (Either String Text)
    }
