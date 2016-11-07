-- Description:
--   Ein paar weitere kleine Antworten von matthias

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   hubot bash me - Frag' hubot nach einem random Zitat link
--   hubot random <min> <max> - Frag' hubot nach einer Zufallszahl zwischen <min> und <max> (nutzt random.org)

-- Author:
--   kiliankoe
module Random where


import           Control.Lens
import           Data.Text      (strip)
import           Marvin.Prelude
import           Network.Wreq


script :: IsAdapter a => ScriptInit a
script = defineScript "random" $ do
    -- robot.hear /.*/, (msg) ->
    --   user = msg.message.user.name.toLowerCase()
    --   if user == donny
    --     msg.send msg.random walter_quotes

    hear (r [CaseInsensitive] "matthias ist (.*)") $ do
        match <- getMatch
        let raw_msg = match `indexEx` 1
        let adj = toLower raw_msg
        unless ("die tür" `isInfixOf` adj || "die tuer" `isInfixOf` adj) $
            reply $ toStrict $ format "Deine Mudda ist {}!" [raw_msg]

    hear (r [CaseInsensitive] "matthias,? du bist (.*)") $ do
        match <- getMatch
        let adj = toLower $ match `indexEx` 1
        reply $ toStrict $ format "Deine Mudda ist {}!" [adj]

    hear (r [CaseInsensitive] "matthias scheißt auf (.*)") $ do
        match <- getMatch
        let term = match `indexEx` 1
        reply $ toStrict $ format "Deine Mudda scheißt auf {}!" [term]

    hear (r [CaseInsensitive] "bash me") $ do
        highestQuote <- fromMaybe 946 <$> getConfigVal "bash-latest"
        rand <- randomValFromRange (1, highestQuote :: Int)
        send $ toStrict $ format "http://bash.fsrleaks.de/?{}" [rand]

    respond (r [CaseInsensitive] "random (\\d*) (\\d*)") $ do
        (_:min:max:_) <- getMatch

        r <- liftIO $ get $ unpack $ "https://www.random.org/integers/?num=1&min=" ++ min ++ "&max=" ++ max ++ "&format=plain&col=1&base=10"
        send $ strip $ decodeUtf8 $ toStrict $ r^.responseBody

donny = "slackbot"
walter_quotes =
    [ "Shut the fuck up, " ++ donny ++ "."
    , "Forget it, " ++ donny ++ ", you're out of your element!"
    , donny ++ ", you're out of your element!"
    , donny ++ ", shut the f—"
    , "That's ex-- Shut the fuck up, " ++ donny ++ "!"
    ]
