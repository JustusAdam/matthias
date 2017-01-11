-- Description:
--   FSR Krams

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   hubot protokoll - Frag' hubot nach dem aktuellem protokoll
--   hubot protokoll <datum> - Frag' hubot nach dem Protokoll von <datum> (ISO-Format)
--   hubot ese - hubot sagt dir das Datum der ESE (falls das jemand updated)
--   hubot jemand da? - hubot sagt dir, ob aktuell wer im Büro ist.
--   hubot buerostatus - hubot zeigt den buerostatus graphen der letzten 6 Stunden.

-- Author:
--   kiliankoe

module FSR where


import           Control.Lens
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Char
import           Data.List
import qualified Data.Text.Lazy             as L
import           Data.Time
import           Marvin.Prelude
import           Network.Wreq


getDay (_, _, day) = day


script :: IsAdapter a => ScriptInit a
script = defineScript "fsr" $ do
    respond (r [CaseInsensitive] "protokoll(.*)") $ do
        match <- getMatch
        case match of
            (_:"":_) -> send "https://www.ifsr.de/protokolle/current.pdf"
            [_] -> send "https://www.ifsr.de/protokolle/current.pdf"
            (_:date:_) ->
                case toGregorian <$> parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) (L.unpack date) of
                    Nothing -> logErrorN $(isT "Unparseable date #{date}")
                    Just dateobj | getDay dateobj /= 1 -> send "Das war leider kein Sitzungsdatum."
                    Just (year, _, day) -> send $(isL "https://www.ifsr.de/protokolle/#{drop 1 $ show year}/#{L.drop 1 date}.pdf")

    respond (r [CaseInsensitive] "ese") $ do
        currentdate <- utctDay <$> liftIO getCurrentTime
        esedate <- getConfigVal "esedate"
        case esedate >>= parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) of
            Nothing -> logErrorN "date not present or wrong format"
            Just esedate -> do
                let datediff = diffDays currentdate esedate

                send $(isL "Nur noch #{datediff} Tage bis zur ESE 2016. Vermutlich :stuck_out_tongue_winking_eye:")

    respond (r [CaseInsensitive] "((wer|jemand) (da|im (büro|buero))|licht an)\\?") $ do
        r <- liftIO $ get "https://www.ifsr.de/buerostatus/output.php"
        send $ case dropWhile isSpace $ dropWhileEnd isSpace $ unpack $ r^.responseBody of
                  "1" -> "Scheint so."
                  "0" -> "Glaub nicht."
                  _ -> "Keine Ahnung, Sebastian hat schon wieder unerwartet was geändert!"

    respond (r [CaseInsensitive] "(buero|büro)(status)?") $
        send "https://www.ifsr.de/buerostatus/image.php?h=6"

    hear (r [CaseInsensitive] "(sind wir (beschlussfähig|beschlussfaehig))") $
        -- How about a list of the elected memebers and then check if enough are online in slack? :P
        send "Einmal durchzählen, bitte! Ich fang' an, 0!"
