-- Description:
--   sudo

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   hubot sudo - Zwing hubot dazu, etwas zu tun

-- Author:
--   kiliankoe
module SudoGer where

import Marvin.Prelude

script :: IsAdapter a => ScriptInit a
script = defineScript "sudo_ger" $
    respond (r [CaseInsensitive] "sudo (.*)") $ do
        match <- getMatch
        let befehl = match `indexEx` 1
        send $ "Ok! Ich " ++ befehl
