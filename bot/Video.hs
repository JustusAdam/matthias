-- Description:
--   Google video search

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   hubot video me <query> - search Google for videos


-- Author:
--   lucaswo

module Video where

import Marvin.Prelude
import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens


script :: IsAdapter a => ScriptInit a
script = defineScript "video" $
    respond (r [CaseInsensitive] "video( me)? (.*)") $ do
        (_:_:query:_) <- getMatch
        let opts = defaults
                       & param "v" .~ ["1.0"]
                       & param "rsz" .~ ["8"]
                       & param "q" .~ [query]
                       & param "safe" .~ ["active"]
        r <- liftIO $ getWith opts "https://ajax.googleapis.com/ajax/services/search/video"

        if r^.responseStatus.statusCode == 200
            then 
                case r^.responseBody ^? key "responseData" . key "results" . _Array of
                    Just v | not $ null v -> do
                        one <- randomFrom v
                        maybe (return ()) send $ one ^? key "url" . _String
                    _ -> return ()

            else send "Bad response status"