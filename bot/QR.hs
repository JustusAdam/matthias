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
import Control.Lens.Aeson
import Data.Aeson

script :: IsAdapter a => ScriptInit a
script = defineScript "qr" $ 
    respond (r [CaseInsensitive] "qr (.*)") $ do
        match <- getMatch
        let url = encodeURIComponent $ msg `indexEx` 1
        r <- liftIO $ get $ "https://api.qrserver.com/v1/read-qr-code/?fileurl=" ++ unpack url
        case eitherDecode' (r^.responseBody) :: Either String Value of
            Left err -> errorM $ "Unreadable json " ++ pack err
            Right data_ ->
                send $ json ^?! nth 0 . key "symbol" . nth 0 . key "data"
