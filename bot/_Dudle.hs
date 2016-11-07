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

module Dudle where


import Marvin.Prelude
import Network.Wreq
import Control.Lens
import Data.Time
import System.Cron
import Text.XML
import Text.XML.Cursor


default_dudle_db = "./data/dudle.json"


dudle_db = fromMaybe default_dudle_db <$> getConfigVal "db"


data Dudle = Dudle
    { dShortname :: Text
    , dUrl :: Text
    , dLastChecked :: UTCTime
    }


script :: IsAdapter a => ScriptInit a
script = defineScript "dudle" $ do

    check_all_dudles <- extractAction $ do
        dudles <- read_dudles_file
        for_ dudles $ \dudle ->
            check_dudle_feed dudles, dudle, (events) ->
                if events.length > 0
                    publish_events robot, dudle, events
        write_dudles_file dudles

    void $ liftIO $ execSchedule $ do
        addJob check_all_dudles "00 00 9 * * *"

    respond (r [CaseInsensitive] "subscribe (.*)") $ do
        (_:match:_) <- getMatch
        -- shortname_from_link returns same content early if it's not a link
        let shortname = shortname_from_link match
            dudle_link = make_valid_dudle_link match
        -- TODO: Theoretically one could validate the dudle itself here as well.
        -- Otherwise it will get removed after <10 minutes automatically
        -- But the user won't be any smarter...
        isSubbed <- is_already_subscribed shortname
        if isSubbed
            then send "Das tracke ich bereits."
            else do
                save_dudle_to_file shortname dudle_link
                send $ "Ok, ich poste Updates zu " ++ shortname ++ " in #dudle."

    respond (r [CaseInsensitive] "unsubscribe (.*)") $ do
        (_:shortname:_) <- getMatch
        found <- remove_dudle_from_file shortname
        send $ if found
                  then "Ok, tracke " ++ shortname ++ " nicht mehr."
                  else "Tracke aktuell nichts mit diesem Namen."

    respond (r [CaseInsensitive] "dudlelist") $ do
        send "Aktuell tracke ich folgende Dudles:"
        dudles <- read_dudles_file
        for_ dudles $ \Dudle{dShortname, dUrl} ->
            send $ dShortname ++ ": " ++ dUrl

    respond (r [CaseInsensitive] "dudle (.*)") $ do
        (_:shortname:_) <- getMatch
        let dudle_link = "https://dudle.inf.tu-dresden.de/" ++ shortname ++ "/"
        r <- get dudle_link
        if r^.responseStatus.statusCode /= 200
            then send "Sieht nicht so aus, als ob das Dudle (noch) existiert."
            else
                case parse_totals $ r^.responseBody of
                    Left err -> errorM err >> send (pack err)
                    Right v | size v == 0 -> send "Das Dudle scheint leer zu sein..."
                    Right v -> for_ v $ send . toStrict . format "Option {} mit {} Stimmen"

shortname_from_link link
    | not $ is_dudle_url link = link
    | has_trailing_slash link = lastSegment $ initEx link
    | otherwise = lastSegment link
  where
    lastSegment = last $ split "/"

make_valid_dudle_link str
    | is_dudle_url str =
        if has_trailing_slash str then str else str ++ "/"
    | otherwise = "https://dudle.inf.tu-dresden.de/" ++ str ++ "/"

is_dudle_url = isJust . match pattern
  where pattern = r [] "dudle\\.inf\\." :: Regex

has_trailing_slash = ("/" `isSuffixOf`)

is_already_subscribed = do
    file <- read_dudles_file
    return $ shortname `member` file

read_dudles_file = do
    dudle_loc <-dudle_db
    exists <- liftIO $ doesFileExist dudle_loc
    if not exists
        then errorM ("Couldnt find dudle file \"" ++ dudle_loc ++ "\"") >> return mempty
        else do
            file <- readFile dudle_loc
            case eitherDecode file of
                Right f -> return f
                Left err -> errorM err >> return mempty

write_dudles_file dudles = do
    dudleLoc <- dudle_db
    writeFile dudleLoc $ encode dudles

save_dudle_to_file shortname url = do
    dudles <- read_dudles_file
    date <- liftIO $ getCurrentTime
    let new_dudle = Dudle
            { dShortname = shortname
            , dUrl = url
            , dLastChecked = date
            }
    write_dudles_file $ insert shortname new_dudle dudles

remove_dudle_from_file shortname = do
    dudles <- read_dudles_file
    write_dudles_file $ delete shortname dudles
    return $ shortname `member` dudles

update_dudle_date shortname date = do
    dudles <- read_dudles_file
    write_dudles_file $ adjust (\d@Dudle{dLastChecked} -> d {dLastChecked = date}) dudles

check_all_dudles robot = do
    dudles <- read_dudles_file
    mapM (check_dudle_feed dudles publish) dudles >>= write_dudles_file


check_dudle_feed map event_callback dudle = do
    let atom_url = url dudle ++ "atom.cgi"
    articles <- feed atom_url

    -- if err != null
    --     console.log "Feed for dudle #{dudle.shortname} not found. Removing it from list."
    --     map.delete dudle.shortname
    last_checked = new Date(dudle.last_checked)
    last_updated = articles.first().published
    now = new Date()
    now.addHours 1
    dudle.last_checked = now
    event_callback(articles
                    .filter((article) -> last_checked < article.published)
                    .map((article) -> article.title))


-- TODO Handle empty events
publish_events dudle events
    | null events = return ()
    | otherwise = do
        messageRoom "#dudle" $ "Neues zum Dudle " ++ dShortname dudle ++ ":"
        events = reverse events # let's do this in chronological order
        for_ events $ \e ->
            messageRoom "#dudle" e
        messageRoom "#dudle" $ "Mehr Infos direkt hier: " ++ url dudle

parse_totals = do
    hi <- header_items
    return $ mapFromList $ zip hi summary_items
  where
    -- TODO Handle errors
    doc = parseLBS_ def body
    cursor = fromDocument doc
    header_rows = (cursor $// attributeIs "class" "participanttable" &// element "thead" &// element "tr" >=> child) `index` 2 >>= ($| child)
--    header_rows = $('#participanttable > thead > tr:nth-child(2)').children()
    header_items = elements_of_column <$> header_rows

    summary_rows = cursor $// attributeIs "class" "summary" &| child
    summary_items = elements_of_column summary_rows



elements_of_column row = do
    map (($| content) . head) $ tail row
    Array.from(row.slice(1, row.length - 1)).map((column) -> column.children[0].data)
