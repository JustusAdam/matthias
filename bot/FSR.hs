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


import Marvin.Prelude
import Data.Time
import Network.Wreq
import Control.Lens hiding (index)
import Data.Text (strip)


getDay (_, _, day) = day


script :: IsAdapter a => ScriptInit a
script = defineScript "fsr" $ do
    respond (r [CaseInsensitive] "protokoll(.*)") $ do
        match <- getMatch
        case match `index` 1 of
            Just "" -> send "https://www.ifsr.de/protokolle/current.pdf"
            Nothing -> send "https://www.ifsr.de/protokolle/current.pdf"
            Just date -> 
                case toGregorian <$> parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) (unpack date) of
                    Nothing -> errorM $ "Unparseable date " ++ date
                    Just dateobj | getDay dateobj /= 1 -> send "Das war leider kein Sitzungsdatum."
                    Just (year, _, day) -> send $ toStrict $ format "https://www.ifsr.de/protokolle/{}/#{date.slice(1, date.length)}.pdf" (drop 1 $ show year, drop 1 date)

    respond (r [CaseInsensitive] "ese") $ do
        currentdate <- utctDay <$> liftIO getCurrentTime
        esedate <- getConfigVal "esedate"
        case esedate >>= parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) of
            Nothing -> errorM "date not present or wrong format"
            Just esedate -> do 
                let datediff = diffDays currentdate esedate  

                send $ toStrict $ format "Nur noch {} Tage bis zur ESE 2016. Vermutlich :stuck_out_tongue_winking_eye:" [datediff]

    respond (r [CaseInsensitive] "(wer|jemand) (da|im (büro|buero))|licht an)\\?") $ do
        r <- liftIO $ get "https://www.ifsr.de/buerostatus/output.php"
        case strip $ toStrict $ decodeUtf8 $ r^.responseBody of
            "1" -> send "Scheint so."
            "0" -> send "Glaub nicht."
            _ -> send "Keine Ahnung, Sebastian hat schon wieder unerwartet was geändert!"  

    respond (r [CaseInsensitive] "(buero|büro)(status)?") $
        send "https://www.ifsr.de/buerostatus/image.php?h=6"

    hear (r [CaseInsensitive] "(sind wir (beschlussfähig|beschlussfaehig))") $
        -- How about a list of the elected memebers and then check if enough are online in slack? :P
        send "Einmal durchzählen, bitte! Ich fang' an, 0!"
