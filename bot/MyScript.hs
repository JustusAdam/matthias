module MyScript where


import           Marvin.Adapter.Slack
import           Marvin.Prelude


script :: ScriptInit SlackRTMAdapter
script = defineScript "my-script" $ do
    hear (r [caseless] "ping") $ do -- react to any message
        msg <- getMessage -- read the message contents
        infoM (content msg) -- logging
        send "Pong" -- sending messages back
    respond "hello" $ do -- react to direct commands
        reply "Hello to you too"
