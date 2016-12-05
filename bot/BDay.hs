-- Description:
--   Happy Birthday \o/

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   hubot birthday? - Who's is the next upcoming birthday?
--   hubot birthday <name> - When's <name>'s birthday?
--   hubot birthday list - List all known birthdays.

-- Author:
--   kiliankoe
--   Justus Adam <me@justus.science>

-- fs = require('fs')
-- cronjob = require("cron").CronJob
-- moment = require('moment')
-- moment.locale('de_DE')

module BDay where

import Marvin.Prelude
import Marvin.Util.JSON
import Data.Time
import System.Cron
import Data.HashMap.Strict (HashMap, toList, mapWithKey)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (for_)
import Control.Monad
import Control.Lens
import Data.Char
import Data.List (intercalate)

dontShow = 1900


defaultBdayDatabase = "data/bday.json"


type Birthdays = HashMap String Day


script :: IsAdapter a => ScriptInit a
script = defineScript "bday" $ do
    bdayDBFile <- fromMaybe defaultBdayDatabase <$> getConfigVal "db"
    bdayFile <- liftIO $ B.readFile bdayDBFile
    bdays <- case eitherDecode' bdayFile of
                Left err -> do
                    errorM $ "could not read bdays.json: " ++ err
                    return mempty
                Right b -> return b

    congratulate <- extractAction $
        for_ (toList (bdays :: Birthdays)) $ \(name, bday) -> do
            today <- liftIO getCurrentTime
            let
                (year, month, day) = toGregorian $ utctDay today
                (bDayYear, bDayMonth, bDayDay) = toGregorian bday
            when (bDayMonth == month && day == bDayDay) $ messageRoom "#random" $ printf ":tada: Alles Gute zum Geburtstag, %v! :tada:" (ix 0 %~ toUpper $ name)
    
    void $ liftIO $ execSchedule $
        addJob congratulate "00 00 9 * * *"    

    respond (r [caseless] "(birthday|bday|geburtstag)\\??$") $ do
        today <- utctDay <$> liftIO getCurrentTime
        let
            (yearToday, _, _) = toGregorian today
            vallist = flip mapWithKey bdays $ \ name value ->
                let
                    (_, bdayMonth, bdayDay) = toGregorian value
                    date = fromGregorian yearToday bdayMonth bdayDay
                in  if date < today
                        then addGregorianYearsRollOver 1 date
                        else date

            date = minimum $ map snd $ toList vallist

            birthdayBoysAndGirls = map ((ix 0 %~ toUpper) . fst) $ filter ((== date)  . snd) $ toList vallist

            daysDiff = diffDays date today
            last_ = last birthdayBoysAndGirls

            diffStr
                | daysDiff == 0 = "heute!"
                | otherwise = printf "in nur %v Tagen." daysDiff

            msgStr = 
                case birthdayBoysAndGirls of
                    [first] -> printf "Das nächste Geburtstagskind ist %v" first
                    _ -> "Die nächsten Geburstage sind von " ++ intercalate ", " (init birthdayBoysAndGirls) ++ " und " ++ last_
        send $ msgStr ++ " " ++ diffStr

    respond (r [caseless] "(birthday|bday|geburtstag) (.+)") $ do
        (_:name':_) <- getMatch
        let name = map toLower name

        case name of
            -- Looks like we're going for the list below...
            "list" -> return ()
            "liste" -> return ()
            _ ->  case HM.lookup name bdays of
                    Just bday -> formatBirthdayInfo name bday >>= send
                    Nothing -> send $ printf "Sorry, ich kenne keinen Geburtstag von %v." name

    respond (r [caseless] "(birthday|bday|geburtstag) list(e?)") $ do
        send "Ich kenne folgende Geburtstage:"
        for_ (toList bdays) $ \(key, value) ->
            formatBirthdayInfo key value >>= send


formatBirthdayInfo :: MonadIO m => String -> Day -> m String
formatBirthdayInfo name birthday
    | birthdayYear == dontShow = return $ printf "%v hat am %v.%v. Geburtstag." (ix 0 %~ toUpper $ name) birthdayDay birthdayMonth
    | otherwise = do
        today <- utctDay <$> liftIO getCurrentTime
        let (age, _, _) = toGregorian $ ModifiedJulianDay $ diffDays today birthday 
        return $ printf "%v wurde am %v.%v. geboren. Das war vor %v Jahren! :O" (ix 0 %~ toUpper $ name) birthdayDay birthdayMonth age
  where
    (birthdayYear, birthdayMonth, birthdayDay) = toGregorian birthday
