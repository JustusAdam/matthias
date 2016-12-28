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
import           Marvin.Prelude
import           Network.Wreq
import Data.Char
import Data.List
import Data.String.Utils
import Data.ByteString.Lazy.Char8 (unpack)


script :: IsAdapter a => ScriptInit a
script = defineScript "random" $ do
    -- robot.hear /.*/, (msg) ->
    --   user = msg.message.user.name.toLowerCase()
    --   if user == donny
    --     msg.send msg.random walter_quotes

    hear (r [caseless] "matthias ist (.*)") $ do
        match <- getMatch
        let raw_msg = match !! 1
        let adj = map toLower raw_msg
        unless ("die tür" `isInfixOf` adj || "die tuer" `isInfixOf` adj) $
            reply $ printf "Deine Mudda ist %v!" raw_msg

    hear (r [caseless] "matthias,? du bist (.*)") $ do
        match <- getMatch
        let adj = map toLower $ match !! 1
        reply $ printf "Deine Mudda ist %v!" adj

    hear (r [caseless] "matthias scheißt auf (.*)") $ do
        match <- getMatch
        let term = match !! 1
        reply $ printf "Deine Mudda scheißt auf %v!" term

    hear (r [caseless] "bash me") $ do
        highestQuote <- fromMaybe 946 <$> getConfigVal "bash-latest"
        rand <- randomValFromRange (1, highestQuote :: Int)
        send $ printf "http://bash.fsrleaks.de/?%v" rand

    respond (r [caseless] "random (\\d*) (\\d*)") $ do
        (_:min:max:_) <- getMatch

        r <- liftIO $ get $ "https://www.random.org/integers/?num=1&min=" ++ min ++ "&max=" ++ max ++ "&format=plain&col=1&base=10"
        send $ strip $ unpack $ r^.responseBody

donny = "slackbot"
walter_quotes =
    [ "Shut the fuck up, " ++ donny ++ "."
    , "Forget it, " ++ donny ++ ", you're out of your element!"
    , donny ++ ", you're out of your element!"
    , donny ++ ", shut the f—"
    , "That's ex-- Shut the fuck up, " ++ donny ++ "!"
    ]
