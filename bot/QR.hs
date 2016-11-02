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

import Marvin.Prelude
import Network.Wreq
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens

script :: IsAdapter a => ScriptInit a
script = defineScript "qr" $ 
    respond (r [CaseInsensitive] "qr (.*)") $ do
        match <- getMatch
        let url = encodeURIComponent $ match `indexEx` 1 :: Text
        r <- liftIO $ get $ "https://api.qrserver.com/v1/read-qr-code/?fileurl=" ++ unpack url
        case eitherDecode' (r^.responseBody) :: Either String Value of
            Left err -> errorM $ "Unreadable json " ++ pack err
            Right json ->
                send $ json ^?! nth 0 . key "symbol" . nth 0 . key "data" . _String
  where encodeURIComponent = undefined
