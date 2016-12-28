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

script :: IsAdapter a => ScriptInit a
script = defineScript "version" $
    respond (r [caseless] "version") $
        send $ "Ich bin Matthias @ https://github.com/fsr/matthias/commit/" ++ $(gitHash)
