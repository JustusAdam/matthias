-- Description:
--   Einfache trigger-response Interaktionen

-- Dependencies:
--   None

-- Configuration:
--   None

-- Commands:
--   hubot pizza - hubot nennt alle Details für eine Pizzabestellung (im Ratsaal)
--   hubot filmlist - Frag' hubot nach Links zu beiden Filmlisten


-- Author:
--   kiliankoe

module SimpleResponses where


import           Marvin.Prelude


simpleresponses =
    [ ("pizza", "Ich bin Matthias Stuhlbein, Nöthnitzer Str. 46, 01187 Dresden. Fakultät Informatik. Mail: pizza@ifsr.de, Telefon: 0351 46338223 - Pizzen schneiden nicht vergessen ;)")
    , ("filmlist", "Vorschläge: http://letterboxd.com/kiliankoe/list/ifsr-filmvorschlage/\nGeschaute Filme: http://letterboxd.com/kiliankoe/list/ifsr-movie-night/")
    , ("wat is wacken\\?", "Dat ist Wacken. Einmal im Jahr kommen hier alle bösen schwarzen Männer aus Mittelerde her, um ma richtig die Sau rauszulassen.")
    , ("marco\\s*", "POLO")
    ]

simplelistens =
    [ ("pimmel", "Höhöhö, du hast Pimmel gesagt.")
    , ("anyway", "how's your sex life?")
    , ("jehova", "http://i.imgur.com/01PMBGj.gif")
    , ("muss man wissen|fefe|sascha lobo|axel stoll", "http://i.imgur.com/FmEyA8t.png")
    , ("you're tearing me apart|the room|tommy wiseau", "http://i.giphy.com/pTrgmCL2Iabg4.gif")
    , ("citrix|35000|35\\.000|35k", ":moneybag::moneybag::moneybag:")
    , ("madness", "Madness you say? THIS. IS. PATRI... MATTHIAS!")
    , ("^nein$", "Doch!")
    , ("\\sphp|^php", "Naja, ich hab mit PHP auch schon richtig gut funktioni⍰\nParse error: syntax error, unexpected '::' (T_PAAMAYIM_NEKUDOTAYIM) in Command line code on line 1")
    , ("gewitter", "Gewitter? In Neuss?")
    , ("danke matthias", "No problemo")
    , ("thanks matthias", "Not sure if sarcastic or actually grateful...")
    , ("thx matthias", "Not sure if sarcastic or actually grateful...")
    , ("(spiel|game)", "ICH HAB' DAS SPIEL VERLOREN!")
    ]


script :: IsAdapter a => ScriptInit a
script = defineScript "simpleresponses" $ do
    for_ simplelistens $ \(trigger, answer) -> do
        hear (r [caseless] trigger) $ do
            rand <- randomValFromRange (0, 5)
            when (rand == (0 :: Int)) $ send answer

    for_ simpleresponses $ \(trigger, answer) -> respond (r [caseless] trigger) $ send answer

