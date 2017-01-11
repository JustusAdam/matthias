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


import qualified Data.Text.Lazy as L
import           Marvin.Prelude


script :: IsAdapter a => ScriptInit a
script = defineScript "drsommer" $ do
    hear (r [CaseInsensitive] "^(hey |hallo |lieber )?dr\\.? sommer,?") $ do
        msg <- getMessage
        name <- randomFrom names
        i <- randomValFromRange (10, 15)
        messageChannel "#dr_sommer" $(isL "\"${msg}\" - #{name} (#{i :: Int})")

    -- help matthias respond to priv messages as well
    respond (r [CaseInsensitive] "(hey |hallo |lieber )?dr\\.? sommer,?") $ do
        msg <- getMessage
        let question = L.replace msg "matthias " ""
        name <- randomFrom names
        i <- randomValFromRange (10, 15)
        messageChannel "#dr_sommer" $(isL "\"#{question}\" - #{name} (#{i :: Int})")


names :: [L.Text]
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
