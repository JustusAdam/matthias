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

import qualified Data.Text.Lazy            as L
import qualified Data.Text.Lazy.Encoding   as L
import           Marvin.Interpolate.String


script :: IsAdapter a => ScriptInit a
script = defineScript "random" $ do
    hear ".*" $ do
        user <- getUser >>= getUsername

        when (user == donny) $ do
            rand <- randomFrom walterQuotes
            send rand

    hear (r [CaseInsensitive] "matthias ist (.*)") $ do
        match <- getMatch
        let raw_msg = match !! 1
        let adj = L.toLower raw_msg
        unless ("die tür" `L.isInfixOf` adj || "die tuer" `L.isInfixOf` adj) $
            reply $(isL "Deine Mudda ist #{raw_msg}!")

    hear (r [CaseInsensitive] "matthias,? du bist (.*)") $ do
        match <- getMatch
        let adj = L.toLower $ match !! 1
        reply $(isL "Deine Mudda ist #{adj}!")

    hear (r [CaseInsensitive] "matthias scheißt auf (.*)") $ do
        match <- getMatch
        let term = match !! 1
        reply $(isL "Deine Mudda scheißt auf #{term}!")

    hear (r [CaseInsensitive] "bash me") $ do
        highestQuote <- fromMaybe 946 <$> getConfigVal "bash-latest"
        rand <- randomValFromRange (1, highestQuote :: Int)
        send $(isL "http://bash.fsrleaks.de/?#{rand}")

    respond (r [CaseInsensitive] "random (\\d*) (\\d*)") $ do
        (_:min:max:_) <- getMatch

        r <- liftIO $ get $(isS "https://www.random.org/integers/?num=1&min=#{min}&max=#{max}&format=plain&col=1&base=10")
        send $ L.strip $ L.decodeUtf8 $ r^.responseBody

donny :: L.Text
donny = "slackbot"
walterQuotes =
    [ $(isL "Shut the fuck up, #{donny}.")
    , $(isL "Forget it, #{donny}, you're out of your element!")
    , $(isL "#{donny}, you're out of your element!")
    , $(isL "#{donny}, shut the f—")
    , $(isL "That's ex-- Shut the fuck up, #{donny}!")
    ]
