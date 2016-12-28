-- Description:
--   dudle

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   hubot subscribe <dudle> - Poste Updates zu <dudle> in #dudle
--   hubot unsubscribe <dudle> - Poste keine weiteren Updates zu <dudle>
--   hubot dudlelist - Welche Dudles werden aktuell beobachtet?
--   hubot dudle <dudle> - Hole aktuelle Ergebnisse zu <dudle>

-- Author:
--   kiliankoe

-- fs = require 'fs'
-- cronjob = require('cron').CronJob
-- cheerio = require 'cheerio'
-- feed = require 'feed-read'
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TemplateHaskell #-}
module Dudle where


import           Control.Lens     hiding (element, index)
import           Data.Maybe       (fromJust, isJust, mapMaybe)
import           Data.Time
import           Marvin.Prelude
import           Network.Wreq     hiding (delete)
import           System.Cron
import           Text.Feed.Import
import           Text.Feed.Query
import           Text.XML         hiding (readFile, writeFile)
import           Text.XML.Cursor  as Cursor
import System.Directory
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Control.Monad
import Data.Text (unpack)
import Data.List
import Marvin.Adapter.Slack


defaultDudleDB = "./data/dudle.json"


dudleDB :: BotReacting a b FilePath
dudleDB = fromMaybe defaultDudleDB <$> getConfigVal "db"


data Dudle = Dudle
    { dShortname   :: String
    , dUrl         :: String
    , dLastChecked :: UTCTime
    }


type Dudles = HM.HashMap String Dudle


deriveJSON defaultOptions {fieldLabelModifier = tail} ''Dudle


script :: ScriptInit SlackRTMAdapter
script = defineScript "dudle" $ do

    checkAllDudles_IO <- extractAction checkAllDudles

    void $ liftIO $ execSchedule $
        addJob checkAllDudles_IO "00 00 9 * * *"

    respond (r [caseless] "subscribe (.*)") $ do
        (_:match:_) <- getMatch
        -- shortnameFromLink returns same content early if it's not a link
        let shortname = shortnameFromLink match
            dudle_link = makeValidDudleLink match
        -- TODO: Theoretically one could validate the dudle itself here as well.
        -- Otherwise it will get removed after <10 minutes automatically
        -- But the user won't be any smarter...
        isSubbed <- isAlreadySubscribed shortname
        if isSubbed
            then send "Das tracke ich bereits."
            else do
                saveDudleToFile shortname dudle_link
                send $ "Ok, ich poste Updates zu " ++ shortname ++ " in #dudle."

    respond (r [caseless] "unsubscribe (.*)") $ do
        (_:shortname:_) <- getMatch
        found <- removeDudleFromFile shortname
        send $ if found
                  then "Ok, tracke " ++ shortname ++ " nicht mehr."
                  else "Tracke aktuell nichts mit diesem Namen."

    respond (r [caseless] "dudlelist") $ do
        send "Aktuell tracke ich folgende Dudles:"
        dudles <- readDudlesFile
        for_ dudles $ \Dudle{dShortname, dUrl} ->
            send $ dShortname ++ ": " ++ dUrl

    respond (r [caseless] "dudle (.*)") $ do
        (_:shortname:_) <- getMatch
        let dudle_link = "https://dudle.inf.tu-dresden.de/" ++ shortname ++ "/"
        r <- liftIO $ get dudle_link
        if r^.responseStatus.statusCode /= 200
            then send "Sieht nicht so aus, als ob das Dudle (noch) existiert."
            else
                case parseTotal $ r^.responseBody of
                    Nothing -> errorM "Dudle unparseable" >> send "Dudle unparseable"
                    Just v | null v -> send "Das Dudle scheint leer zu sein..."
                    Just v -> for_ v $ \(opt, count) -> send $ printf "Option {} mit {} Stimmen" opt count

shortnameFromLink link
    | not $ isDudleUrl link = link
    | hasTrailingSlash link = lastSegment $ init link
    | otherwise = lastSegment link
  where
    lastSegment = dropWhileEnd (/= '/')

makeValidDudleLink str
    | isDudleUrl str =
        if hasTrailingSlash str then str else str ++ "/"
    | otherwise = "https://dudle.inf.tu-dresden.de/" ++ str ++ "/"

isDudleUrl = isJust . match [] pat
  where pat = r [] "dudle\\.inf\\." :: Regex

hasTrailingSlash = ("/" `isSuffixOf`)

isAlreadySubscribed shortname = do
    file <- readDudlesFile
    return $ shortname `HM.member` (file :: Dudles)

readDudlesFile = do
    dudle_loc <- dudleDB
    exists <- liftIO $ doesFileExist dudle_loc
    if not exists
        then errorM ("Couldnt find dudle file \"" ++ dudle_loc ++ "\"") >> return mempty
        else do
            file <- liftIO $ BL.readFile dudle_loc
            case eitherDecode file of
                Right f  -> return f
                Left err -> errorM err >> return mempty

withDudlesFile :: (Dudles -> BotReacting a b (Maybe Dudles)) -> BotReacting a b ()
withDudlesFile action = readDudlesFile >>= action >>= maybe (return ()) writeDudlesFile

writeDudlesFile dudles = do
    dudleLoc <- dudleDB
    liftIO $ BL.writeFile dudleLoc $ encode dudles

saveDudleToFile shortname url = withDudlesFile $ \dudles -> do
    date <- liftIO getCurrentTime
    let new_dudle = Dudle
            { dShortname = shortname
            , dUrl = url
            , dLastChecked = date
            }
    return $ Just $ HM.insert shortname new_dudle dudles

removeDudleFromFile shortname = do
    dudles <- readDudlesFile
    writeDudlesFile $ HM.delete shortname dudles
    return $ shortname `HM.member` dudles

updateDudleDate shortname date = withDudlesFile $
    return . Just . HM.adjust (\d@Dudle{dLastChecked} -> d {dLastChecked = date}) shortname

checkAllDudles = withDudlesFile $ \dudles ->
    Just <$> foldM (checkDudleFeed publishEvents) dudles (HM.elems dudles)


checkDudleFeed eventCallback dudles dudle = do
    let atom_url = dUrl dudle ++ "atom.cgi"
    r <- liftIO $ get atom_url
    if r^.responseStatus.statusCode /= 200
        then do
            messageChannel "#dudle" $ "Feed for dudle " ++ dShortname dudle ++ " not found. Removing it from list."
            return $ HM.delete (dShortname dudle) dudles
        else
            case parseFeedSource $ r^.responseBody of
                Nothing -> errorM "Feed unparseable" >> return dudles
                Just feed -> do
                    let articles = getFeedItems feed

                    -- if err != null
                    --     console.log "Feed for dudle #{dudle.shortname} not found. Removing it from list."
                    --     map.delete dudle.shortname
                        last_checked = dLastChecked dudle
                        last_updated = fromJust $ getItemPublishDate $ head articles :: Maybe UTCTime
                    now <- liftIO getCurrentTime
                    let nowLater = addUTCTime (60 * 60) now
                        dudleLater = dudle { dLastChecked = nowLater }
                    eventCallback
                        dudleLater
                        $ articles
                            & filter ((< last_checked) . fromJust . join . getItemPublishDate)
                            & mapMaybe getItemTitle
                    return $ HM.insert (dShortname dudle) dudleLater dudles


-- TODO Handle empty events
publishEvents dudle events
    | null events = return ()
    | otherwise = do
        messageChannel "#dudle" $ "Neues zum Dudle " ++ dShortname dudle ++ ":"
        let eventsChron = reverse events -- let's do this in chronological order
        for_ eventsChron $ messageChannel "#dudle"
        messageChannel "#dudle" $ "Mehr Infos direkt hier: " ++ dUrl dudle

parseTotal :: BL.ByteString -> Maybe [(String, String)]
parseTotal body = do
    hi <- header_items
    return $ zip hi summary_items
  where
    -- TODO Handle errors
    doc = parseLBS_ def body
    cursor = fromDocument doc
    header_rows :: Maybe [Cursor.Cursor]
    header_rows = ($| child) <$> (tr $| child :: [Cursor]) `index` 2
      where
        [tr] = cursor $// attributeIs "class" "participanttable" &// element "thead" &// element "tr"
--    header_rows = $('#participanttable > thead > tr:nth-child(2)').children()
    header_items = elementsOfColumn <$> header_rows

    summary_rows = join $ cursor $// attributeIs "class" "summary" &| child
    summary_items = elementsOfColumn summary_rows


index :: [a] -> Int -> Maybe a
index [] _ = Nothing
index l i = Just $ l !! i

elementsOfColumn :: [Cursor] -> [String]
elementsOfColumn = map (unpack . head . ($| Cursor.content)) . tail
