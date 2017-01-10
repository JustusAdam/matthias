-- Description:
--   version

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   hubot version - Welche Version von hubot läuft gerade?

-- Author:
--   kiliankoe
{-# LANGUAGE TemplateHaskell #-}
module Version where

import           Development.GitRev
import           Marvin.Prelude

hash :: String
hash = $(gitHash)

script :: IsAdapter a => ScriptInit a
script = defineScript "version" $
    respond (r [CaseInsensitive] "version") $
        send $(isL "Ich bin Matthias @ https://github.com/fsr/matthias/commit/#{hash}")
