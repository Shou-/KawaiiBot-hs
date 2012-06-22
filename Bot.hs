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

{-# LANGUAGE DoAndIfThenElse, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Bot where

import Utils
import Config

import Codec.Binary.UTF8.String (decodeString)
import Control.Exception
import Data.List (isInfixOf)
import Data.Foldable (foldr')
import Data.Maybe (fromJust, listToMaybe)
import Data.String.Utils (split, join, strip, replace)
import Network.Curl
import System.IO (Handle)
import System.Random (randomRIO)
import Text.Regex (subRegex, mkRegex)
import Text.JSON
import Text.XML.Light

-- Interpret messages and change the data into whatever the functions return
-- Splits the message on bind operators and uses recursion to execute all as separate functions
-- The boolean returned decides whether it's a destination message (True) or user message (False).
msgInterpret :: Meta -> String -> IO (Bool, String)
msgInterpret meta str =
    if words str /= [] then do
        let carg    = split ":" $ (!! 0) $ words str
            cmd     = (!! 0) carg
            smesg   = unwords . takeWhile notAnyOper . tail $ words str
            opers   = dropWhile notAnyOper $ words str
            postIO :: String -> IO (Bool, String)
            postIO x
                | x `isFunc` [">"] = -- Public message
                    return (True, smesg)
                | x `isFunc` ["<"] = -- Private message
                    return (False, smesg)
                | x `isFunc` ["alias"] = -- Alias
                    return (True, [])
                | x `isFunc` ["calc"] = -- Calculator
                    return (calc smesg) >>= return . (tuple2 True)
                | x `isFunc` ["help", "?"] = -- Help about commands
                    return (False, help smesg)
                | x `isFunc` ["lewd"] = -- Lewd messages
                    lewd meta >>= return . (tuple2 False)
                | x `isFunc` ["wiki"] = -- Wikipedia summary
                    wiki smesg >>= return . (tuple2 True)
                | x `isFunc` ["sed"] = -- Regex replace
                    return (True, sed smesg)
                | x `isFunc` ["ai"] = -- Recently airing anime
                    airing carg >>= return . (tuple2 True)
                | x `isFunc` ["an"] = -- Recent anime / anime search
                    anime smesg carg >>= return . (tuple2 True)
                | x `isFunc` ["fi"] = -- Filter
                    return (True, [])
                | x `isFunc` ["ma"] = -- Recent manga / manga search
                    smesg `manga` carg >>= return . (tuple2 True)
                | x `isFunc` ["pi"] = -- Pickpocket
                    return (True, [])
                | x `isFunc` ["ra"] = -- Random/choice
                    random smesg >>= return . (tuple2 True)
                | x `isFunc` ["$"] = -- Variable storing
                    variable meta carg (bisectAt ' ' smesg) >>= return . (tuple2 True)
                | x `isFunc` ["re"] = -- Reminder, alias for $
                    variable meta ["$", "reminder"] (getUsernick meta ++ "-reminder", smesg) >>= return . (tuple2 True)
                | x `isFunc` ["tr"] = -- Translate
                    translate carg smesg >>= return . (tuple2 True)
                | x `isFunc` ["us"] = -- Userlist
                    return (False, []) -- (True, userlistShow meta)
                | x `isFunc` ["we"] = -- Weather
                    weather smesg >>= return . (tuple2 True)
                | x `isFunc` ["^"] = -- Last message / message history
                    lastMsg meta smesg >>= return . (tuple2 True)
                | head x == '\SOH' = -- CTCP interpreter
                    return . (tuple2 False) $ ctcpInterpret str
                | otherwise = -- Fallback
                    print str >> return (True, [])
            handlebinds :: [String] -> IO (Bool, String)
            handlebinds [] = postIO cmd
            handlebinds (x:xs)
                | x == ">>" = do
                    (_, post) <- postIO cmd
                    let act     = unwords xs
                    putStrLn $ "Act: " ++ show act ++ "\nPost: " ++ show post
                    msgI <- msgInterpret meta act
                    return msgI
                | x == "->" = do
                    (_, post) <- postIO cmd
                    let func     = unwords . takeWhile notAnyOper $ xs
                        funcTail = unwords . dropWhile notAnyOper $ xs
                    putStrLn $ "Act: " ++ show func ++ "\nPost: " ++ show post ++ "\nAct tail: " ++ show funcTail
                    msgI <- msgInterpret meta $ func ++ ' ' : post ++ ' ' : funcTail
                    return msgI
                | x == "++" = do
                    (_, post) <- postIO cmd
                    let add     = unwords . takeWhile notAnyOper $ xs
                        addTail = (' ':) . unwords . dropWhile notAnyOper $ xs
                    putStrLn $ "Act: " ++ show add ++ "\nPost: " ++ show post ++ "\nAct tail: " ++ show addTail
                    (msgI1, msgI2) <- msgInterpret meta $ add ++ addTail
                    return (msgI1, post ++ " " ++ msgI2)
                | otherwise = postIO cmd
        handlebinds opers
    else return (True, [])
  where notAnyOper        = (\x -> not $ x `elem` [">>", "->", "++"])
        tuple2 x y = (x, y)
        isFunc x y = x `elem` [v : y' | y' <- y, v <- ['.', '!']]

ctcpInterpret :: String -> String
ctcpInterpret ('\SOH':xs)
    | take 7 xs == "VERSION" = "VERSION KawaiiBot 0.1"
    | otherwise = []
ctcpInterpret _ = []

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
anime' :: String -> [String] -> IO [String]
anime' str args = do
    let sepFilter :: String -> (String, [String]) -> (String, [String])
        sepFilter x (stracc, filacc) = if negateWord x
            then (stracc, tail x : filacc)
            else (' ' : x ++ stracc, filacc)
        (str', filters) = foldr' sepFilter ("", []) $ words str
        url = "https://tokyotosho.info/search.php?type=1&terms=" ++ urlEncode' str'
    html <- curlGetString' url []
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "a" Nothing Nothing
        attrs = [Attr (QName "class" Nothing Nothing) "desc-top"]
        elems = findElementsAttrs elem' qname attrs
        animes = map (strip . elemsText) elems
        animes' = filter (\x -> not . or $ map (`isInfixOf` x) filters) animes
        amount = if length args > 1 then (args !! 1) `safeRead` 10 else 10 :: Int
    putStrLn $ "Filters: %(f); str': %(s)" % [("f", join ", " filters), ("s", str')]
    return $ take amount animes'
  where negateWord :: String -> Bool
        negateWord ('-':_) = True
        negateWord _       = False

-- Styled anime function
anime :: String -> [String] -> IO String
anime str args = do
    animes <- anime' str args
    let animes' = map (replace "[" "[\ETX13" . replace "]" "\ETX]") animes
    return $ joinUntil ", " animes' 400

-- Receive, parse and output recent anime airing times.
airing :: [String] -> IO String
airing args = do
    html <- curlGetString' ("http://www.mahou.org/Showtime/Showtime.html") []
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "tr" Nothing Nothing
        elems = findElements qname elem'
        animes = map elemsText $ tail elems
        amount = if length args > 1 then (args !! 1) `safeRead` 10 else 10 :: Int
        animes' = take amount $ genAnime animes
    return $ joinUntil ", " animes' 400
  where genAnime :: [String] -> [String]
        genAnime (animetitle:eta:time:_:xs) = istrip (unwords ["\ETX10" ++ strip animetitle ++ "\ETX", "\ETX13" ++ (strip . drop 16) eta ++ "\ETX", "(" ++ drop 10 time ++ ")"]) : genAnime xs
        genAnime _                   = []

-- calculator function
-- This needs to be rewritten to work without spaces and ().
calc :: String -> String
calc _ = [] -- temporary, until it gets fixed
calc [] = []
calc x' =
    let xs = words x'
    in show . parse $ xs
  where parse :: (Read n, Floating n) => [String] -> n
        parse (x:y:xs)  | y == "+" = read x + parse xs
                        | y == "-" = read x - parse xs
                        | y == "*" = read x * parse xs
                        | y == "/" = read x / parse xs
                        | y == "**" = read x ** parse xs
                        | y == "^" = read x ** parse xs
                        | otherwise = 0 + parse xs
        parse (x:[]) = read x
        parse [] = 0

-- Information about the IRC commands the bot provides
-- Incomplete
help :: String -> String
help s
    | s == "bind"       = "Bind, or >>, is an operator that performs the function preceding it, but disregards the value returned."
    | s == "calc"       = "Calculates some numbers with a horrible function which only vaguely follows the order of operation. Ex: .calc 2 + 5 * 2"
    | s == "lewd"       = "Prints a lewd message."
    | s == "pipe"       = "Pipe, or ->, is an operator that takes the value of what precedes it and appends it to the argument of the next function."
    | s == "wiki"       = "Prints the top of a Wikipedia article. Ex: .wiki haskell programming language"
    | s == "add"        = "Add, or ++, is an operator that takes the value of what precedes it and appends it to the value of the next function."
    | s == "sed"        = "Replaces text using regular expressions. Ex: .sed /probably should/should not/ You probably should help it."
    | s == "ai"         = "Prints anime airing in the near future. Ex: .ai:10"
    | s == "an"         = "Prints recent anime releases, can also be used to search. Ex: .an:3 Last Exile -Commie"
    | s == "ma"         = "Prints recent manga releases, can also be used to search. Ex: .ma:5 Banana no Nana"
    | s == "ra"         = "Prints a number from a range or a string choice split by `|'. Ex: .ra 1000; Ex: .ra Yes | No"
    | s == "re"         = "An alias for .$:reminder reminder-%(nick). Ex: .re Watch Welcome to the NHK."
    | s == "tr"         = "Translate a string. Ex: .tr:ja:en 日本語"
    | s == "we"         = "Prints the weather for a specified location. Ex: .we Tokyo"
    | s == ">"          = "Prints a string to the channel. Ex: .> hey"
    | s == "<"          = "Prints a string to the user. Ex: .< hey"
    | s == "^"          = "Gets a message from the history. Ex: .^ 3"
    | s == "$"          = "Gets a stored variable. Ex: .$:immutable myVariableName My variable message."
    | otherwise         = "Try `.help' and a function: >, <, ^, $, calc, wiki, sed, ai, an, ma, ra, tr, us, we. Or an operator: bind, pipe, add."

-- Read str as Int, or 0 if not number. Use it as an index for the history list. Fallback to `[]'.
lastMsg :: Meta -> String -> IO String
lastMsg meta str =
    let history = []
        len = length history
        n = if isNum . strip $ str then read str else 0 :: Int
    in if len > n
        then return . (\(_, _, _, x) -> x) $ history !! n
        else return []

-- Output lewd strings.
lewd :: Meta -> IO String
lewd meta = do
    file <- readFile lewdPath
    randomadj1 <- randomRIO (0, pred $ length adj1s)
    let obj = [("nick", getUsernick meta), ("adj1", adj1s !! randomadj1)]
        lewds = map (\x -> read x % obj) . lines $ file
    randomlewd <- randomRIO (0, pred $ length lewds)
    return $ lewds !! randomlewd
  where adj1s = [ "hard"
                , "wet"
                , "delicious"
                , "pink"
                , "sticky"]

-- Receive, parse and output recent, or specific, manga titles and related information.
manga :: String -> [String] -> IO String
manga str cargs = do
    let url = "https://www.mangaupdates.com/releases.html?act=archive&search=" ++ urlEncode' str
    html <- curlGetString' url []
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
random :: String -> IO String
random str
    | isNum str = randomRIO (0, read str :: Integer) >>= return . show
    | otherwise =
        let choices = split "|" str
            len = length choices
            choice = randomRIO (0, len - 1) >>= return . (choices !!)
        in choice

-- Receive, parse and output the title, or otherwise specified text.
title :: FilePath -> IO String
title url = do
    let opts = [CurlFollowLocation True,
                CurlTimeout 5,
                CurlMaxFileSize (1024 * 1024 * 5),
                CurlUserAgent $ unwords [   "Mozilla/5.0",
                                            "(X11; Linux x86_64;",
                                            "rv:10.0.2)",
                                            "Gecko/20100101",
                                            "Firefox/10.0.2"]]

    response <- curlGetResponse url opts
    let headers = respHeaders response
        contentTypeResp = headers `tupleListGet` "Content-Type"
        contentResp = respBody response
    if (take 9 . strip) contentTypeResp == "text/html" then do
        let maybeElems = parseXMLDoc contentResp
            elems = if isNothing maybeElems
                        then Element (QName "" Nothing Nothing) [] [] Nothing
                        else fromJust maybeElems
            getTitle = filterElements (\element -> if (qName . elName $ element) == "title" then True else False) elems
            special = (if not . isNothing $ specialElem
                            then (++ " ") . elemsText . head' . (fromJust specialElem) $ elems 
                            else []) `cutoff` 200
            pagetitle = ((special ++ " ") ++) . unwords . map elemsText $ getTitle
        return . replace "\n" " " . istrip $ pagetitle
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
--translate :: [String] -> String -> IO String
translate args str = do
    let from = show $ if length args > 1 then args !! 1 else ""
        to = show $ if length args > 2 then args !! 2 else "en"
        str' = show [urlEncode' str]
        url = concat [  "http://api.microsofttranslator.com/",
                        "v2/ajax.svc/TranslateArray",
                        "?appId=" ++ show msAppId,
                        "&texts=" ++ str',
                        "&from=" ++ from,
                        "&to=" ++ to ]
    jsonStr <- ((\_ -> return "") :: SomeException -> IO String) `handle` curlGetString' url []
    putStrLn url >> putStrLn jsonStr

    let jsonStr' = dropWhile (/= '[') jsonStr
        result = decode jsonStr' :: Result [JSObject JSValue]
        result' = fmap (fmap snd . (`tupleMaybeGet` "TranslatedText") . fromJSObject . (!! 0)) result
        text = fromMaybeString $ fmap fromJSString (getOut result')
    return text
  where getOut (Ok (Just (JSString a))) = Just a
        getOut _ = Nothing

-- Variable
-- YOU'RE WORKING ON THIS YOU だめ人間; FINISH THIS SHIT.
-- Need to work on the ``event'' function to finish this.
variable :: Meta -> [String] -> (String, String) -> IO String
variable meta [_] (varname, []) = do -- Read variable
    file <- handle ((\_ -> return "") :: SomeException -> IO String)
                   (readFile variablePath)
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
variable meta args (varname, str) = do -- Set variable
    contents <- handle ((\_ -> return "") :: SomeException -> IO String)
                       (readFile variablePath)
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

    if writeableVar $ msg
        then putStrLn ("It's writeable: " ++ show variables') >>
             writeVariables variables'
        else if nick == nick'
            then putStrLn ("Not writeable, but same user: " ++ show variables') >>
                 writeVariables variables'
            else putStrLn "Cannot overwrite variable." >> return []
  where バカ ('p':'r':'i':_) = Personal
        バカ ('i':'m':'m':_) = Immutable
        バカ ('r':'e':'m':_) = Reminder
        バカ _               = Regular
        writeVariables :: Variables -> IO String
        writeVariables variables' = do
            writeStatus <- try (writeFile variablePath $ show variables') :: IO (Either SomeException ())
            case writeStatus of
                Left e -> return . show $ e
                Right _ -> return ""

weather :: String -> IO String
weather str = do
    xml <- curlGetString' ("http://www.google.com/ig/api?weather=" ++ urlEncode' str) []
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

-- Event function
-- Incomplete
event :: Handle -> String -> Meta -> IO ()
event h meta str = return ()

-- Wikipedia
-- Incomplete -- fix this
wiki :: String -> IO String
wiki s = do
    html <- curlGetString' ("https://en.wikipedia.org/w/index.php?search=%s&title=Special%3ASearch" % (replace "+" "%20" . urlEncode') s) []
    let element = fromMaybeElement $ parseXMLDoc html
        qname = QName "div" (Just "http://www.w3.org/1999/xhtml") (Just "http://www.w3.org/1999/xhtml")
        attrs = [Attr (QName "id" Nothing Nothing) "mw-content-text"]
        element' = fromMaybeElement . listToMaybe $ findElementsAttrs element qname attrs
        qname' = QName "p" Nothing Nothing
        element'' = fromMaybeElement $ findElement qname' element'
        text = elemsText element''
    return $ cutoff text 300
