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
import Marvin.Interpolate.String

script :: IsAdapter a => ScriptInit a
script = defineScript "qr" $
    respond (r [CaseInsensitive] "qr (.*)") $ do
        match <- getMatch
        let url = encodeURIComponent $ match !! 1 :: String
        r <- liftIO $ get $(isS "https://api.qrserver.com/v1/read-qr-code/?fileurl=#{url}")
        case eitherDecode' (r^.responseBody) :: Either String Value of
            Left err -> logErrorN $(isT "Unreadable json #{err}")
            Right json ->
                send $(isL "#{json ^?! nth 0 . key \"symbol\" . nth 0 . key \"data\" . _String}")
  where encodeURIComponent = undefined
