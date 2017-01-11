-- Description:
--   geruecht

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   hubot gerücht - Lass' hubot aus dem Nähkästchen plaudern

-- Author:
--   kiliankoe

-- cheerio = require 'cheerio'

module Geruecht where

import           Control.Lens
import           Data.Text                 (replace, strip, unpack)
import           Data.Text.Lazy            (fromStrict)
import           Marvin.Interpolate.String
import           Marvin.Prelude
import           Network.Wreq
import           Text.XML
import           Text.XML.Cursor           as C


script :: IsAdapter a => ScriptInit a
script = defineScript "gerücht" $
    respond (r [CaseInsensitive] "geruecht|gerücht") $ do
        user <- requireConfigVal "username"
        pw <- requireConfigVal "password"
        r <- liftIO $ get $(isS "http://#{user :: String}:#{pw :: String}@leaks.fsrleaks.de/index.php")

        let cursor = fromDocument $ parseLBS_ def $ r^.responseBody
            txt = head $ cursor $// C.element "div" &/ C.content
            replaced = strip $ replace "Psst..." "" txt

        rand <- randomFrom prefixes

        send $ fromStrict $ rand `mappend` replaced

            -- .get() (err, res, body) ->
            --     $ = cheerio.load body
            --     geruecht = $('div').text().replace /Psst\.\.\./, ""
            --     geruecht = geruecht.trim()
            --     geruecht = msg.random(prefixes) + geruecht
            --     msg.send geruecht

prefixes = [
    "Also... Ich hab ja Folgendes gehört: ",
    "Pssht... ",
    "Ein kleines Vögelchen hat mir das hier verraten: ",
    "Die BILD Zeitung soll ja das hier morgen als Titelstory haben: ",
    "Böse Zungen behaupten ja: ",
    "Bei Fefe stand letzte Woche: ",
    "Also bei Sebi in der BRAVO Girl stand ja letztens: "
    ]
