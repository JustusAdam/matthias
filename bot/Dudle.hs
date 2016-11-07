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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Dudle where


import Marvin.Prelude
import Network.Wreq hiding (delete)
import Control.Lens hiding (index, element)
import Data.Time
import System.Cron
import Text.XML hiding (writeFile, readFile)
import Text.XML.Cursor as Cursor
import Text.Feed.Query
import Text.Feed.Import
import Data.Maybe (fromJust)


defaultDudleDB = "./data/dudle.json"


dudleDB = fromMaybe defaultDudleDB <$> getConfigVal "db"


data Dudle = Dudle
    { dShortname :: Text
    , dUrl :: Text
    , dLastChecked :: UTCTime
    }


script :: IsAdapter a => ScriptInit a
script = defineScript "dudle" $ do

    checkAllDudles_IO <- checkAllDudles

    void $ liftIO $ execSchedule $
        addJob checkAllDudles_IO "00 00 9 * * *"

    respond (r [CaseInsensitive] "subscribe (.*)") $ do
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

    respond (r [CaseInsensitive] "unsubscribe (.*)") $ do
        (_:shortname:_) <- getMatch
        found <- removeDudleFromFile shortname
        send $ if found
                  then "Ok, tracke " ++ shortname ++ " nicht mehr."
                  else "Tracke aktuell nichts mit diesem Namen."

    respond (r [CaseInsensitive] "dudlelist") $ do
        send "Aktuell tracke ich folgende Dudles:"
        dudles <- readDudlesFile
        for_ dudles $ \Dudle{dShortname, dUrl} ->
            send $ dShortname ++ ": " ++ dUrl

    respond (r [CaseInsensitive] "dudle (.*)") $ do
        (_:shortname:_) <- getMatch
        let dudle_link = "https://dudle.inf.tu-dresden.de/" ++ shortname ++ "/"
        r <- get dudle_link
        if r^.responseStatus.statusCode /= 200
            then send "Sieht nicht so aus, als ob das Dudle (noch) existiert."
            else
                case parseTotals $ r^.responseBody of
                    Left err -> errorM err >> send (pack err)
                    Right v | size v == 0 -> send "Das Dudle scheint leer zu sein..."
                    Right v -> for_ v $ send . toStrict . format "Option {} mit {} Stimmen"

shortnameFromLink link
    | not $ isDudleUrl link = link
    | hasTrailingSlash link = lastSegment $ initEx link
    | otherwise = lastSegment link
  where
    lastSegment = last $ split "/"

makeValidDudleLink str
    | isDudleUrl str =
        if hasTrailingSlash str then str else str ++ "/"
    | otherwise = "https://dudle.inf.tu-dresden.de/" ++ str ++ "/"

isDudleUrl = isJust . match pat
  where pat = r [] "dudle\\.inf\\." :: Regex

hasTrailingSlash = ("/" `isSuffixOf`)

isAlreadySubscribed = do
    file <- readDudlesFile
    return $ shortname `member` file

readDudlesFile = do
    dudle_loc <-dudleDB
    exists <- liftIO $ doesFileExist dudle_loc
    if not exists
        then errorM ("Couldnt find dudle file \"" ++ pack dudle_loc ++ "\"") >> return mempty
        else do
            file <- readFile dudle_loc
            case eitherDecode file of
                Right f -> return f
                Left err -> errorM (pack err) >> return mempty

withDudlesFile action = readDudlesFile >>= action >>= maybe (return ()) writeDudlesFile

writeDudlesFile dudles = do
    dudleLoc <- dudleDB
    writeFile dudleLoc $ encode dudles

saveDudleToFile shortname url = withDudlesFile $ \dudles -> do
    date <- liftIO getCurrentTime
    let new_dudle = Dudle
            { dShortname = shortname
            , dUrl = url
            , dLastChecked = date
            }
    insertMap shortname new_dudle dudles

removeDudleFromFile shortname = do
    dudles <- readDudlesFile
    writeDudlesFile $ delete shortname dudles
    return $ shortname `member` dudles

updateDudleDate shortname date = withDudlesFile $
    return . Just . adjustMap (\d@Dudle{dLastChecked} -> d {dLastChecked = date})

checkAllDudles = withDudlesFile $ \dudles ->
    Just <$> foldM (checkDudleFeed publishEvents) dudles dudles


checkDudleFeed eventCallback dudles dudle = do
    let atom_url = unpack $ dUrl dudle ++ "atom.cgi"
    r <- liftIO $ get atom_url
    if r^.responseStatus.statusCode /= 200
        then do
            send $ "Feed for dudle " ++ dShortname dudle ++ " not found. Removing it from list."
            return $ delete (dShortname dudle) dudles
        else
            case parseFeedSource $ r^.responseBody of
                Nothing -> errorM "Feed unparseable" >> return dudles
                Just feed -> do
                    let articles = getFeedItems feed

                    -- if err != null
                    --     console.log "Feed for dudle #{dudle.shortname} not found. Removing it from list."
                    --     map.delete dudle.shortname
                        last_checked = dLastChecked dudle
                        last_updated = fromJust $ getItemPublishDate $ headEx articles
                    now <- liftIO getCurrentTime
                    let nowLater = now + (60 * 60)
                        dudleLater = dudle { dLastChecked = nowLater }
                    eventCallback
                        dudleLater
                        $ articles
                            & filter ((< last_checked) . fromJust . join . getItemPublishDate)
                            & map getItemTitle
                    return $ insertMap (dShortname dudle) dudleLater dudles


-- TODO Handle empty events
publishEvents dudle events
    | null events = return ()
    | otherwise = do
        messageRoom "#dudle" $ "Neues zum Dudle " ++ dShortname dudle ++ ":"
        let eventsChron = reverse events -- let's do this in chronological order
        for_ eventsChron $ messageRoom "#dudle"
        messageRoom "#dudle" $ "Mehr Infos direkt hier: " ++ dUrl dudle

parseTotal body = do
    hi <- header_items
    return $ mapFromList $ zip hi summary_items
  where
    -- TODO Handle errors
    doc = parseLBS_ def body
    cursor = fromDocument doc
    header_rows :: Maybe [Cursor.Cursor]
    header_rows = (tr $| child :: [Cursor]) `index` 2 >>= ($| child)
      where
        [tr] = cursor $// attributeIs "class" "participanttable" &// element "thead" &// element "tr"
--    header_rows = $('#participanttable > thead > tr:nth-child(2)').children()
    header_items = elementsOfColumn <$> header_rows

    summary_rows = cursor $// attributeIs "class" "summary" &| child
    summary_items = elementsOfColumn summary_rows



elementsOfColumn :: (IsSequence (s Cursor), Functor s) => s Cursor -> s Text
elementsOfColumn = map (unsafeHead . ($| Cursor.content)) . unsafeTail
