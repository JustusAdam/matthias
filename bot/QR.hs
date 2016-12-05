-- Description:
--   Decode QR codes

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   hubot qr <qr_url> - Dekodiere einen QR Code

-- Author:
--   kiliankoe

module QR where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Marvin.Prelude
import           Network.Wreq
import Data.Text (unpack)

script :: IsAdapter a => ScriptInit a
script = defineScript "qr" $
    respond (r [caseless] "qr (.*)") $ do
        match <- getMatch
        let url = encodeURIComponent $ match !! 1
        r <- liftIO $ get $ "https://api.qrserver.com/v1/read-qr-code/?fileurl=" ++ url
        case eitherDecode' (r^.responseBody) :: Either String Value of
            Left err -> errorM $ "Unreadable json " ++ err
            Right json ->
                send $ unpack $ json ^?! nth 0 . key "symbol" . nth 0 . key "data" . _String
  where encodeURIComponent = undefined
