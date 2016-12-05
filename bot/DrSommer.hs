-- Description:
--   Dr. Sommer

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   Dr. Sommer, ich hab da mal ne Frage - Stelle eine anonyme Frage in #dr_sommer

-- Author:
--   kiliankoe
module DrSommer where


import Marvin.Prelude
import Data.List.Extra


script :: IsAdapter a => ScriptInit a
script = defineScript "drsommer" $ do
    hear (r [caseless] "^(hey |hallo |lieber )?dr\\.? sommer,?") $ do
        msg <- getMessage
        name <- randomFrom names
        i <- randomValFromRange (10, 15)
        messageRoom "#dr_sommer" $ printf "\"%v\" - %v (%v)" (content msg) name (i :: Int)

    -- help matthias respond to priv messages as well
    respond (r [caseless] "(hey |hallo |lieber )?dr\\.? sommer,?") $ do
        msg <- getMessage
        let question = replace (content msg) "matthias " ""
        name <- randomFrom names
        i <- randomValFromRange (10, 15)
        messageRoom "#dr_sommer" $ printf "\"%v\" - %v (%v)" question name (i :: Int)


names :: [String]
names = [
    "Shantalle",
    "Schackeline",
    "Tamara",
    "Cindy",
    "Jenny",
    "Clair-Gina",
    "Justin",
    "Jerome",
    "Chantal",
    "Jacqueline",
    "Yves",
    "Sylvia",
    "Claudia",
    "Dörte"
    ]
