{-
A lewd IRC bot that does useless things.
Copyright (C) 2012 Shou

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DoAndIfThenElse
           , BangPatterns
           #-}

module Bot where

import Types
import Utils

import Control.Exception
import Control.Monad hiding (join)
import Control.Monad.Reader hiding (join)
import Data.List (isInfixOf)
import Data.Foldable (foldr')
import Data.Maybe (fromJust, listToMaybe)
import Data.String.Utils
import Network.Curl
import System.IO
import System.Random (randomRIO)
import Text.Regex (subRegex, mkRegex)
import Text.JSON
import Text.XML.Light


-- Interpret messages and change the data into whatever the functions return
-- Splits the message on bind operators and uses recursion to execute all as separate functions
-- The boolean returned decides whether it's a destination message (True) or user message (False).
msgInterpret :: String -- String received from the IRC server
             -> Memory Message
msgInterpret str =
    if words str /= [] then do
        verbosity <- asks (verbosityC . getConfig)
        let cmd       = takeWhile (/= ':') $ takeWhile (/= ' ') str
            carg      = concat . fmap (split ":") . take 1 . words $ str
            smesg     = unwords . takeWhile notAnyOper . tail $ words str
            opers     = dropWhile notAnyOper $ words str
            postIO :: String -> Memory Message
            postIO x
                | x `isFunc` [">"] = -- Public message
                    return $ ChannelMsg smesg
                | x `isFunc` ["<"] = -- Private message
                    return $ UserMsg smesg
                | x `isFunc` ["help", "?"] = -- Help about commands
                    return . UserMsg $ help smesg
                | x `isFunc` ["isup"] = -- return isup.me's title
                    let url = "http://isup.me/" ++ removePrefixes ["https://", "http://"] smesg
                    in title url >>= return . ChannelMsg . replace "Huh" "Nyaa" . unwords . take 4 . words
                | x `isFunc` ["lewd"] = -- Lewd messages
                    lewd >>= return . UserMsg
                | x `isFunc` ["wiki"] = -- Wikipedia summary
                    wiki smesg >>= return . ChannelMsg
                | x `isFunc` ["sed"] = do -- Regex replace
                    e <- liftIO (try (return $ sed smesg) :: IO (Either SomeException String))
                    case e of
                        Right e -> return $ ChannelMsg e
                        Left e -> liftIO (print e) >> return EmptyMsg
                | x `isFunc` ["ai"] = -- Recently airing anime
                    airing carg >>= return . ChannelMsg
                | x `isFunc` ["an"] = -- Recent anime / anime search
                    anime smesg carg >>= return . ChannelMsg
                | x `isFunc` ["ma"] = -- Recent manga / manga search
                    manga smesg carg >>= return . ChannelMsg
                | x `isFunc` ["ra"] = -- Random/choice
                    random smesg >>= return . ChannelMsg
                | x `isFunc` ["$"] = -- Variable storing
                    variable carg (bisectAt ' ' smesg) >>= return . ChannelMsg
                | x `isFunc` ["tr"] = -- Translate
                    translate carg smesg >>= return . ChannelMsg
                | x `isFunc` ["we"] = -- Weather
                    weather smesg >>= return . ChannelMsg
                | x `isFunc` ["^"] = -- Last message / message history
                    lastMsg smesg >>= return . ChannelMsg
                | isCTCP x = -- CTCP message
                    let xs = tail x
                    in case () of
                      _ | take 7 (tail x) == "VERSION" -> return . UserMsg $ "VERSION "++ botversion
                        | otherwise -> return EmptyMsg
                | otherwise = -- Fallback
                    liftIO (print str) >> return EmptyMsg
            handlebinds :: [String] -> Memory Message
            handlebinds [] = postIO cmd
            handlebinds (x:xs)
                | x == ">>" = do
                    -- add laziness to this
                    -- check if it's necessary to execute it or not
                    post <- fmap fromMsg $ postIO cmd
                    let act = unwords xs
                    liftIO $ when (verbosity > 1) $ putStrLn $ "Act: " ++ show act ++ "\nPost: " ++ show post
                    msgI <- msgInterpret act
                    return msgI
                | x == "->" = do
                    post <- fmap fromMsg $ postIO cmd
                    let func     = unwords . takeWhile notAnyOper $ xs
                        funcTail = unwords . dropWhile notAnyOper $ xs
                    liftIO . when (verbosity > 1) $ do
                        putStrLn $ "Act: " ++ show func ++ "\nPost: " ++ show post ++ "\nAct tail: " ++ show funcTail
                    msgI <- msgInterpret $ func ++ ' ' : post ++ ' ' : funcTail
                    return msgI
                | x == "++" = do
                    post <- fmap fromMsg $ postIO cmd
                    let add     = unwords . takeWhile notAnyOper $ xs
                        addTail = (' ':) . unwords . dropWhile notAnyOper $ xs
                    liftIO . when (verbosity > 1) $ do
                        putStrLn $ "Act: " ++ show add ++ "\nPost: " ++ show post ++ "\nAct tail: " ++ show addTail
                    msgI <- msgInterpret $ add ++ addTail
                    return (msgType msgI $ post ++ " " ++ fromMsg msgI)
                | otherwise = postIO cmd
        handlebinds opers
    else return EmptyMsg
  where notAnyOper = (\x -> not $ x `elem` [">>", "->", "++"])
        isFunc x y = x `elem` [v : y' | y' <- y, v <- ['.', '!']]
        isCTCP ('\SOH':xs) = True
        isCTCP _ = False

-- Holy fukc this sucks
-- Regex replace function for msgInterpret
sed :: String -> String
sed [] = []
sed (_:s) =
    let escaped = replace "\\/" "\t" . replace "\\\\" "\n" $ s
        escapeds = map (replace "\t" "/") . map (replace "\n" "\\") $ split "/" escaped
    in if length escapeds > 2 then
        let match = escapeds !! 0
            result = escapeds !! 1
            message = tail . join "/" $ drop 2 escapeds
        in subRegex (mkRegex match) message result
    else ""

-- Core anime function
anime' :: String -> [String] -> Memory [String]
anime' str args = do
    let sepFilter :: String -> (String, [String]) -> (String, [String])
        sepFilter x (stracc, filacc) = if negateWord x
            then (stracc, tail x : filacc)
            else (' ' : x ++ stracc, filacc)
        (str', filters) = foldr' sepFilter ("", []) $ words str
        url = "https://tokyotosho.info/search.php?type=1&terms=" ++ urlEncode' str'
    html <- liftIO $ curlGetString' url []
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "a" (Just "http://www.w3.org/1999/xhtml") Nothing
        attrs = [Attr (QName "type" Nothing Nothing) "application/x-bittorrent"]
        elems = findElementsAttrs elem' qname attrs
        animes = map (strip . elemsText) elems
        animes' = filter (\x -> not . or $ map (`isInfixOf` x) filters) animes
        amount = if length args > 1 then (args !! 1) `safeRead` 10 else 10 :: Int
    liftIO . putStrLn . take 100 . show $ elem'
    verbosity <- asks (verbosityC . getConfig)
    liftIO . when (verbosity > 1) $ do
        putStrLn $ "Filters: %(f); str': %(s)" % [("f", join ", " filters), ("s", str')]
    return $ take amount animes'
  where negateWord :: String -> Bool
        negateWord ('-':_) = True
        negateWord _       = False

-- Styled anime function
anime :: String -> [String] -> Memory String
anime str args = do
    animes <- anime' str args
    let animes' = map (replace "[" "[\ETX13" . replace "]" "\ETX]") animes
    return $ joinUntil ", " animes' 400

-- Receive, parse and output recent anime airing times.
airing' :: [String] -> Memory [String]
airing' args = do
    html <- liftIO $ curlGetString' ("http://www.mahou.org/Showtime/Showtime.html") []
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "tr" Nothing Nothing
        elems = findElements qname elem'
        animes = map elemsText $ tail elems
        amount = if length args > 1 then (args !! 1) `safeRead` 10 else 10 :: Int
        animes' = take amount $ genAnime animes
    return animes'
  where genAnime :: [String] -> [String]
        genAnime (animetitle:eta:time:_:xs) = istrip (unwords ["\ETX10" ++ strip animetitle ++ "\ETX", "\ETX13" ++ (strip . drop 16) eta ++ "\ETX", "(" ++ drop 10 time ++ ")"]) : genAnime xs
        genAnime _                   = []

-- Front-end for airing', returning a joined list instead
airing :: [String] -> Memory String
airing args = do
    airs <- airing' args
    return $ joinUntil ", " airs 400

-- Information about the IRC commands the bot provides
-- Incomplete
help :: String -> String
help s
    | s == "bind"       = "Bind, or >>, is an operator that performs the function preceding it, but disregards the value returned."
    | s == "lewd"       = "Prints a lewd message."
    | s == "pipe"       = "Pipe, or ->, is an operator that takes the value of what precedes it and appends it to the argument of the next function."
    | s == "add"        = "Add, or ++, is an operator that takes the value of what precedes it and appends it to the value of the next function."
    | s == "sed"        = "Replaces text using regular expressions. Ex: .sed /probably should/should not/ You probably should help it."
    | s == "ai"         = "Prints anime airing in the near future. Ex: .ai:10"
    | s == "an"         = "Prints recent anime releases, can also be used to search. Ex: .an:3 Last Exile -Commie"
    | s == "ma"         = "Prints recent manga releases, can also be used to search. Ex: .ma:5 Banana no Nana"
    | s == "ra"         = "Prints a number from a range or a string choice split by `|'. Ex: .ra 1000; Ex: .ra Yes | No"
    | s == "tr"         = "Translate a string. Ex: .tr:ja:en 日本語"
    | s == "we"         = "Prints the weather for a specified location. Ex: .we Tokyo"
    | s == ">"          = "Prints a string to the channel. Ex: .> hey"
    | s == "<"          = "Prints a string to the user. Ex: .< hey"
    | s == "^"          = "Gets a message from the history. Ex: .^ 3"
    | s == "$"          = "Gets a stored variable. Ex: .$:immutable myVariableName My variable message."
    | s == "github"     = "https://github.com/Shou-/KawaiiBot-hs"
    | otherwise         = "Try `.help' and a value. Functions: >, <, ^, $, sed, ai, an, ma, ra, tr, we. Operators: bind, pipe, add. Extra: github"

-- Read str as Int, or 0 if not number. Use it as an index for the history list. Fallback to `[]'.
lastMsg :: String -> Memory String
lastMsg str = do
    meta <- asks getMeta
    logsPath <- asks (logsPathC . getConfig)
    let serverurl = getServer meta
        dest = getDestino meta
    e <- liftIO . try $ (do
        let path = logsPath ++ serverurl ++ " " ++ dest
        fmap (map (\x -> read x :: ([String], String)) . reverse . lines) $ readFile path) :: Memory (Either SomeException [([String], String)])
    case e of
        Right history -> do
            let len = length history
                n = if isNum . strip $ str then read str else 0 :: Int
            if len > n
                then return . snd $ history !! n
                else return []
        Left e -> do
            verbosity <- asks (verbosityC . getConfig)
            liftIO . when (verbosity > 1) $ print e
            return []

-- Output lewd strings.
lewd :: Memory String
lewd = do
    meta <- asks getMeta
    lewdPath <- asks (lewdPathC . getConfig)
    file <- liftIO $ readFile lewdPath
    randomadj1 <- liftIO $ randomRIO (0, pred $ length adj1s)
    let obj = [("nick", getUsernick meta), ("adj1", adj1s !! randomadj1)]
        lewds = map (\x -> read x % obj) . lines $ file
    randomlewd <- liftIO $ randomRIO (0, pred $ length lewds)
    return $ lewds !! randomlewd
  where adj1s = [ "hard"
                , "wet"
                , "delicious"
                , "pink"
                , "sticky"]

-- Receive, parse and output recent, or specific, manga titles and related information.
manga :: String -> [String] -> Memory String
manga str cargs = do
    let url = "https://www.mangaupdates.com/releases.html?act=archive&search=" ++ urlEncode' str
    html <- liftIO $ curlGetString' url []
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "td" Nothing Nothing
        attrs = [Attr (QName "class" Nothing Nothing) "text pad"]
        elems = findElementsAttrs elem' qname attrs
        elems' = map elemsText elems
        -- Generate a list of colored strings
        genMangas :: [String] -> [String]
        genMangas (date:mangatitle:vol:chp:group:rest) =
            let mangaStr = concat [ "\x03", "12[", strip group, "]\x03 ",
                                    mangatitle, " [Ch.\x03", "13", chp, "\x03",
                                    "Vol.\x03", "13", vol, "\x03] (\x03", "13",
                                    date, "\x03)"]
            in mangaStr : genMangas rest
        genMangas _ = []
        maybeAmount =
            if length cargs > 1
                then fmap fst . listToMaybe . reads $ cargs !! 1
                else Just 10 :: Maybe Int
        amount =
            if maybeAmount /= Nothing
                then fromJust maybeAmount 
                else 10
        mangas = take amount $ genMangas elems'
    return $ joinUntil ", " mangas 400

-- Pick a random choice or number
random :: String -> Memory String
random str
    | isNum str = liftIO (randomRIO (0, read str :: Integer)) >>= return . show
    | otherwise =
        let choices = split "|" str
            len = length choices
        in liftIO (randomRIO (0, len - 1)) >>= return . (choices !!)

-- Receive, parse and output the title, or otherwise specified text.
title :: MonadIO m => String -> m String
title url = do
    let opts = [ CurlFollowLocation True
               , CurlTimeout 5
               , CurlMaxFileSize (1024 * 1024 * 5)
               , CurlUserAgent $ unwords [ "Mozilla/5.0"
                                         , "(X11; Linux x86_64;"
                                         , "rv:10.0.2)"
                                         , "Gecko/20100101"
                                         , "Firefox/10.0.2"
                                         ]
               ]

    response <- liftIO $ curlGetResponse url opts
    let headers = respHeaders response
        contentTypeResp = headers `tupleListGet` "Content-Type"
        contentResp = respBody response
    if (take 9 . strip) contentTypeResp == "text/html" then do
        let maybeElems = parseXMLDoc contentResp
            elems = if isNothing maybeElems
                        then Element (QName "" Nothing Nothing) [] [] Nothing
                        else fromJust maybeElems
            getTitle :: [Element]
            getTitle = filterElements (\element -> if (qName . elName $ element) == "title" then True else False) elems
            special = (if not . isNothing $ specialElem
                            then (++ " ") . elemsText . head' . (fromJust specialElem) $ elems 
                            else []) `cutoff` 200
            pagetitle = ((special ++ " ") ++) . unwords . map elemsText $ getTitle
        return . istrip . replace "\n" " " $ pagetitle
    else
        return []
  where specialElem :: Maybe (Element -> [Element])
        specialElem
            | '#' `elem` url =
                let elemID = tail $ dropWhile (/= '#') url
                    attrs = [Attr (QName "id" Nothing Nothing) elemID]
                    f x = findElementsAttrs x (QName "" Nothing Nothing) attrs
                in Just f
            | let surl = split "/" url
              in surl !! 2 == "boards.4chan.org" && surl !! 4 == "res" =
                    Just $ findElements (QName "blockquote" Nothing Nothing)
            | otherwise = Nothing
        head' :: [Element] -> Element
        head' (x:_) = x
        head' _     = Element (QName "" Nothing Nothing) [] [] Nothing

-- Translate text to another language
-- TODO: Replace with Microsoft Translate -- U FRUSTRATED U FRUSTRATED GOOGLE Y U SO MAAAAAAAAAAAAAAAAAAD
translate :: [String] -> String -> Memory String
translate args str = do
    msAppId <- asks (msAppIdC . getConfig)
    verbosity <- asks (verbosityC . getConfig)
    case msAppId of
        "" -> return []
        _ -> do
            let from = show $ if length args > 1 then args !! 1 else ""
                to = show $ if length args > 2 then args !! 2 else "en"
                str' = show [urlEncode' str]
                url = concat [  "http://api.microsofttranslator.com/",
                                "v2/ajax.svc/TranslateArray",
                                "?appId=" ++ show msAppId,
                                "&texts=" ++ str',
                                "&from=" ++ from,
                                "&to=" ++ to ]
            jsonStr <- liftIO $ ((\_ -> return "") :: SomeException -> IO String) `handle` curlGetString' url []
            liftIO . when (verbosity > 1) $ putStrLn url >> putStrLn jsonStr

            let jsonStr' = dropWhile (/= '[') jsonStr
                result = decode jsonStr' :: Result [JSObject JSValue]
                result' = fmap (fmap snd . (`tupleMaybeGet` "TranslatedText") . fromJSObject . (!! 0)) result
                text = fromMaybeString $ fmap fromJSString (getOut result')
            return text
  where getOut (Ok (Just (JSString a))) = Just a
        getOut _ = Nothing

-- rewrite `Variable'
-- Add:
---- Return specific variable (Reminder only, etc)
------ Can be solved if you pass a function to it
---- Private variables that are stored under the user's nick
---- and Public variables that are owned by the channel, and duplicates cannot exist

-- Variable
-- YOU'RE WORKING ON THIS YOU だめ人間; FINISH THIS SHIT.
variable :: [String] -> (String, String) -> Memory String
variable [_] (varname, []) = do -- Read variable
    meta <- asks getMeta
    variablePath <- asks (variablePathC . getConfig)
    e <- (liftIO . try $ do
        h <- openFile variablePath ReadMode
        !f <- hGetContents h
        hClose h
        return f) :: Memory (Either SomeException String)
    case e of
        Right file -> do
            let nick = getUsernick meta
                dest = getDestino meta
                server = getServer meta
                variables =
                    let maybeVariables = fmap fst . listToMaybe . reads $ file
                    in if maybeVariables /= Nothing
                        then fromJust maybeVariables
                        else []
                dests = variables `tupleListGet` server
                vars = dests `tupleListGet` dest
                (_, nick', msg) = vars `tupleVarGet` varname
            if msg /= EmptyVar
                then if readableVar msg
                    then return . fromVariable $ msg
                    else if nick == nick'
                        then return . fromVariable $ msg
                        else return []
                else return []
        Left e -> liftIO (print e) >> return []
variable args (varname, str) = do -- Set variable
    meta <- asks getMeta
    variablePath <- asks (variablePathC . getConfig)
    e <- (liftIO . try $ do
        h <- openFile variablePath ReadMode
        !f <- hGetContents h
        hClose h
        return f) :: Memory (Either SomeException String)
    case e of
        Right contents -> do
            let nick = getUsernick meta
                dest = getDestino meta
                server = getServer meta
                variables = let maybeVariables = fmap fst . listToMaybe . reads $ contents
                            in if maybeVariables /= Nothing
                                then fromJust maybeVariables
                                else []
                dests = variables `tupleListGet` server
                vars = dests `tupleListGet` dest
                (_, nick', msg) = vars `tupleVarGet` varname

                vartype = if length args > 1 then バカ (args !! 1) else Regular
                var = vartype str

                nicks' = vars `tupleVarInject` (varname, nick, var)
                dests' = (dest, nicks') `tupleInject` dests
                variables' = (server, dests') `tupleInject` variables :: Variables

            if writeableVar msg
                then do
                    liftIO . putStrLn $ "It's writeable: " ++ show variables'
                    writeVariables variables'
                else if nick == nick'
                    then do
                        liftIO . putStrLn $ "Not writeable, but same user: " ++ show variables'
                        writeVariables variables'
                    else do
                        liftIO $ putStrLn "Cannot overwrite variable."
                        return []
        Left e -> liftIO (print e) >> return []
  where バカ ('p':'r':'i':_) = Personal
        バカ ('i':'m':'m':_) = Immutable
        バカ ('r':'e':'m':_) = Reminder
        バカ _               = Regular
        writeVariables :: Variables -> Memory String
        writeVariables variables' = do
            variablePath <- asks (variablePathC . getConfig)
            writeStatus <- (liftIO . try $ do
                h <- openFile variablePath WriteMode
                hPutStrLn h $ show variables'
                hClose h) :: Memory (Either SomeException ())
            case writeStatus of
                Right _ -> return []
                Left e -> liftIO (print e) >> return []

weather :: String -> Memory String
weather str = do
    xml <- liftIO $ curlGetString' ("http://www.google.com/ig/api?weather=" ++ urlEncode' str) []
    let weatherElem = fromMaybeElement $ parseXMLDoc xml
        city = getData . fromMaybeElement $ findElement (QName "city" Nothing Nothing) weatherElem
        condition = map formatCondition . getConditionData $ findElements (QName "current_conditions" Nothing Nothing) weatherElem
        forecasts = map formatForecast . getConditionData $ findElements (QName "forecast_conditions" Nothing Nothing) weatherElem
    return . join "; " $ condition ++ forecasts
  where getData element = fromMaybeString $ findAttr (QName "data" Nothing Nothing) element
        getConditionData elems = map (map getData . onlyElems . elContent) elems
        formatCondition [condition, tempF, tempC, humidity, _, wind] =
            concat ["\ETX05Today\ETX: ", tempF, "\176F / ", tempC, "\176C, ",
                    join ", " [condition, humidity, wind]]
        formatCondition _ = []
        formatForecast [day, _, _, _, condition] = "\ETX05" ++ day ++ "\ETX: " ++ condition
        formatForecast _ = []

-- Wikipedia
-- Incomplete -- fix this
wiki :: String -> Memory String
wiki s = do
    html <- liftIO $ curlGetString' ("https://en.wikipedia.org/w/index.php?search=%s&title=Special%3ASearch" % (replace "+" "%20" . urlEncode') s) []
    let element = fromMaybeElement $ parseXMLDoc html
        qname = QName "div" (Just "http://www.w3.org/1999/xhtml") Nothing
        attrs = [Attr (QName "id" Nothing Nothing) "mw-content-text"]
        element' = fromMaybeElement . listToMaybe $ findElementsAttrs element qname attrs
        qname' = QName "p" (Just "http://www.w3.org/1999/xhtml") Nothing
        element'' = fromMaybeElement $ findElement qname' element'
        text = elemsText element''
    return . (`cutoff` 400) $ text
