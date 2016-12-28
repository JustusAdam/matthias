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

import Marvin.Prelude
import Network.Wreq
import Control.Lens
import Text.XML.Cursor as C
import Text.XML
import Data.Text (unpack, replace, strip)


script :: IsAdapter a => ScriptInit a
script = defineScript "gerücht" $ do
    respond (r [caseless] "geruecht|gerücht") $ do
        user <- requireConfigVal "username"
        pw <- requireConfigVal "password"
        r <- liftIO $ get $ "http://" ++ user ++ ":" ++ pw ++ "@leaks.fsrleaks.de/index.php"

        let cursor = fromDocument $ parseLBS_ def $ r^.responseBody
            txt = head $ cursor $// C.element "div" &/ C.content
            replaced = unpack $ strip $ replace "Psst..." "" txt
        
        rand <- randomFrom prefixes

        send $ rand ++ replaced

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
