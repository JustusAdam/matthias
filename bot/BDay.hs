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

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import           Data.Char
import           Data.Foldable        (for_)
import           Data.HashMap.Strict  (HashMap, mapWithKey, toList)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (intercalate)
import           Data.Maybe           (fromMaybe)
import qualified Data.Text.Lazy       as L
import           Data.Time
import           Marvin.Prelude
import           Marvin.Util.JSON
import           System.Cron

dontShow = 1900


defaultBdayDatabase = "data/bday.json"


type Birthdays = HashMap L.Text Day


script :: IsAdapter a => ScriptInit a
script = defineScript "bday" $ do
    bdayDBFile <- fromMaybe defaultBdayDatabase <$> getConfigVal "db"
    bdayFile <- readJSON bdayDBFile
    bdays <- case bdayFile of
                Left err -> do
                    logErrorN $(isT "could not read bdays.json: #{err}")
                    return mempty
                Right b -> return b

    congratulate <- extractAction $
        for_ (toList (bdays :: Birthdays)) $ \(name, bday) -> do
            today <- liftIO getCurrentTime
            let
                (year, month, day) = toGregorian $ utctDay today
                (bDayYear, bDayMonth, bDayDay) = toGregorian bday
            when (bDayMonth == month && day == bDayDay) $ messageChannel "#random" $(isL ":tada: Alles Gute zum Geburtstag, #{ix 0 %~ toUpper $ name}! :tada:")

    void $ liftIO $ execSchedule $
        addJob congratulate "00 00 9 * * *"

    respond (r [CaseInsensitive] "(birthday|bday|geburtstag)\\??$") $ do
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
                | otherwise = $(isL "in nur #{daysDiff} Tagen.")

            msgStr =
                case birthdayBoysAndGirls of
                    [first] -> $(isL "Das nächste Geburtstagskind ist #{first}")
                    _ -> $(isL "Die nächsten Geburstage sind von #{L.intercalate \", \" (init birthdayBoysAndGirls)} und #{last_}")
        send $(isL "#{msgStr} #{diffStr}")

    respond (r [CaseInsensitive] "(birthday|bday|geburtstag) (.+)") $ do
        (_:name':_) <- getMatch
        let name = L.toLower name
        -- Looks like we're going for the list below...
        unless (name `elem` ["list", "liste"]) $
            case HM.lookup name bdays of
                Just bday -> formatBirthdayInfo name bday >>= send
                Nothing -> send $(isL "Sorry, ich kenne keinen Geburtstag von #{name}")

    respond (r [CaseInsensitive] "(birthday|bday|geburtstag) list(e?)") $ do
        send "Ich kenne folgende Geburtstage:"
        for_ (toList bdays) $ \(key, value) ->
            formatBirthdayInfo key value >>= send


formatBirthdayInfo :: MonadIO m => L.Text -> Day -> m L.Text
formatBirthdayInfo name birthday
    | birthdayYear == dontShow = return $(isL "#{ix 0 %~ toUpper $ name} hat am #{birthdayDay}.#{birthdayMonth}. Geburtstag.")
    | otherwise = do
        today <- utctDay <$> liftIO getCurrentTime
        let (age, _, _) = toGregorian $ ModifiedJulianDay $ diffDays today birthday
        return $(isL "#{ix 0 %~ toUpper $ name} wurde am #{birthdayDay}.#{birthdayMonth}. geboren. Das war vor #{age} Jahren! :O")
  where
    (birthdayYear, birthdayMonth, birthdayDay) = toGregorian birthday
