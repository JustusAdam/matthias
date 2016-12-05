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
import Control.Lens
import Data.List
import Data.Char
import Data.ByteString.Lazy.Char8 (unpack) 


getDay (_, _, day) = day


script :: IsAdapter a => ScriptInit a
script = defineScript "fsr" $ do
    respond (r [caseless] "protokoll(.*)") $ do
        match <- getMatch
        case match of
            (_:"":_) -> send "https://www.ifsr.de/protokolle/current.pdf"
            [_] -> send "https://www.ifsr.de/protokolle/current.pdf"
            (_:date:_) ->
                case toGregorian <$> parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) date of
                    Nothing -> errorM $ "Unparseable date " ++ date
                    Just dateobj | getDay dateobj /= 1 -> send "Das war leider kein Sitzungsdatum."
                    Just (year, _, day) -> send $ printf "https://www.ifsr.de/protokolle/%v/%v.pdf" (drop 1 $ show year) (drop 1 date)

    respond (r [caseless] "ese") $ do
        currentdate <- utctDay <$> liftIO getCurrentTime
        esedate <- getConfigVal "esedate"
        case esedate >>= parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) of
            Nothing -> errorM "date not present or wrong format"
            Just esedate -> do
                let datediff = diffDays currentdate esedate

                send $ printf "Nur noch %v Tage bis zur ESE 2016. Vermutlich :stuck_out_tongue_winking_eye:" datediff

    respond (r [caseless] "(wer|jemand) (da|im (büro|buero))|licht an)\\?") $ do
        r <- liftIO $ get "https://www.ifsr.de/buerostatus/output.php"
        send $ case dropWhile isSpace $ dropWhileEnd isSpace $ unpack $ r^.responseBody of
                  "1" -> "Scheint so."
                  "0" -> "Glaub nicht."
                  _ -> "Keine Ahnung, Sebastian hat schon wieder unerwartet was geändert!"

    respond (r [caseless] "(buero|büro)(status)?") $
        send "https://www.ifsr.de/buerostatus/image.php?h=6"

    hear (r [caseless] "(sind wir (beschlussfähig|beschlussfaehig))") $
        -- How about a list of the elected memebers and then check if enough are online in slack? :P
        send "Einmal durchzählen, bitte! Ich fang' an, 0!"
