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
{-# OPTIONS_HADDOCK prune #-}

module KawaiiBot.Bot where


import KawaiiBot.Types
import KawaiiBot.Utils

import Control.Applicative
import Control.Exception
import Control.Monad hiding (join)
import qualified Control.Monad as M
import Control.Monad.Reader hiding (join)
import Data.Char
import Data.List
import Data.Foldable (foldr')
import Data.Graph.Inductive.Query.Monad (mapFst, mapSnd)
import Data.Maybe (fromJust, listToMaybe)
import Data.String.Utils
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple
import Network.Curl
import System.IO
import System.Random (randomRIO)
import Text.Regex
import Text.JSON
import Text.XML.Light
import Debug.Trace


-- | Parse a string into Funa data then interpret it, returning the output
-- string. Example:
--
-- @
-- parser \".> I like bananas. -> .sed s\/banana\/apple\/\"
-- @
parser :: String
       -> Memory (Message String)
parser x = interpret . toFuna $ split " " x

-- | Words to Funa data
toFuna :: [String] -> Funa
toFuna xs =
    let (head', tail') = breakLate isFuna xs
        len = length head'
    in case () of
      _ | len < 1 -> Void
        | len == 1 -> Plain $ head head'
        | len > 1 ->
            let op = last head'
            in if isFuna op
                then (readFuna op) (unwords $ init head') $ toFuna tail'
                else Plain $ unwords head'
  where isFuna = (`elem` [">>", "++", "->", "$$"])
        readFuna "++" = Add
        readFuna "->" = Pipe
        readFuna ">>" = Bind
        readFuna "$$" = App

-- | Parse the Funa data and use `run' where necessary.
interpret :: Funa -> Memory (Message String)
interpret Void = return EmptyMsg
interpret (Plain s) = run s
interpret (Add s f) = allowThen allowAdd $ do
    rs <- run s
    ex <- interpret f
    return $ rs `mergeMsg` ex
interpret (Bind s f) = allowThen allowBind $ interpret f
interpret (Pipe s f) = allowThen allowPipe $ run s >>= interpret . deepPipe f
interpret (App s f) = allowThen allowApp $ do
    ex <- fmap fromMsg $ interpret f
    run $ s ++ ex

-- | Run an IRC bot function and return the string.
--
-- @
-- run \".> I like bananas.\"
-- @
run :: String -> Memory (Message String)
run x =
    let (fcmd, fstring) = break (not . (/= ' ')) x
        (cmd : args) = split ":" fcmd <|> ["", ""]
        string = dropWhile (== ' ') fstring
    in case () of
      _ | cmd `isCmd` ">" -> -- Public message
            allowThen allowEcho $
                echo ChannelMsg args string
        | cmd `isCmd` "<" -> -- Private message
            allowThen allowEcho $
                echo UserMsg args string
        | cmd `isCmd` "^" -> -- Last message / message history
            allowThen allowHistory $
                findMsg args string
        | cmd `isCmd` "$" -> -- Variable storing / fetching
            allowThen allowVariable $
                variable2 args string
        | cmd `isCmd` "help" -> -- Help function
            help args string
        | cmd `isCmd` "ai" -> -- Current anime airtimes
            allowThen allowAiring $
                airing args string
        | cmd `isCmd` "an" -> -- Recent anime releases
            allowThen allowAnime $
                anime args string
        | cmd `isCmd` "isup" -> -- Website status
            allowThen allowIsup $
                isup args string
        | cmd `isCmd` "lewd" -> -- Lewd message
            allowThen allowLewd $
                lewd2 args string
        | cmd `isCmd` "ma" -> -- Recent manga releases
            allowThen allowManga $
                manga args string
        | cmd `isCmd` "ra" -> -- Random / choice
            allowThen allowRandom $
                random args string
        | cmd `isCmd` "sed" -> -- Regular expression replace
            allowThen allowSed $
                sed2 args string
        | cmd `isCmd` "tr" -> -- Translate 
            allowThen allowTranslate $
                translate args string
        | cmd `isCmd` "we" -> -- Weather
            allowThen allowWeather $
                weather args string
        | cmd `isCmd` "wiki" -> -- Wikipedia
            allowThen allowWiki $
                wiki args string
        | x `isCTCP` "VERSION" -> -- CTCP VERSION message
                return . UserMsg $ "VERSION " ++ botversion
        | otherwise -> return EmptyMsg
  where isCmd (x:xs) cmd = and $ [x `elem` ".!", xs == cmd]
        isCmd _ _ = False
        isCTCP ('\SOH':xs) y = y `isPrefixOf` xs
        isCTCP _ _ = False

-- | Append a string to a Funa's string.
deepPipe :: Funa -> Message String -> Funa
deepPipe Void _ = Void
deepPipe (Plain s) m = Plain $ unwords [s, fromMsg m]
deepPipe (Add s f) m = Add (unwords [s, fromMsg m]) f
deepPipe (Bind s f) m = Bind (unwords [s, fromMsg m]) f
deepPipe (Pipe s f) m = Pipe (unwords [s, fromMsg m]) f
deepPipe (App s f) m = App (unwords [s, fromMsg m]) f

echo :: (String -> Message String)
     -> [String]
     -> String
     -> Memory (Message String)
echo f _ s = return (f s)

-- | Channel and user message functions.
channelMsg, userMsg :: [String] -> String -> Memory (Message String)
channelMsg = echo ChannelMsg
userMsg = echo UserMsg

-- | Regex replace function.
--
-- > .sed s\/banana\/apple\/
--
sed2 :: [String] -> String -> Memory (Message String)
sed2 _ ('s':x:xs) = do
    let f :: (Bool, (Int, ((String, String), String))) 
          -> Char 
          -> (Bool, (Int, ((String, String), String)))
        f (True, (n, ((a, b), c))) x'
            | n == 1 && x' == x = (False, (n, ((a ++ [x'], b), c)))
            | n == 1 = (False, (n, ((a ++ ['\\', x'], b), c)))
            | n == 2 && x' == x = (False, (n, ((a, b ++ [x']), c)))
            | n == 2 = (False, (n, ((a, b ++ ['\\', x']), c)))
            | otherwise = (False, (n, ((a, b), c ++ ['\\', x'])))
        f (False, (n, ((a, b), c))) x'
            | n == 1 && x' == x = (False, (2, ((a, b), c)))
            | n == 1 && x' == '\\' = (True, (n, ((a, b), c)))
            | n == 1 = (False, (n, ((a ++ [x'], b), c)))
            | n == 2 && x' == x = (False, (3, ((a, b), c)))
            | n == 2 && x == '\\' = (True, (n, ((a, b), c)))
            | n == 2 = (False, (n, ((a, b ++ [x']), c)))
            | otherwise = (False, (n, ((a, b), c ++ [x'])))
        (_, (_, ((match, replacement), string))) = foldl f (False, (1, (("", ""), ""))) xs
        (ins, _:string') = break (== ' ') string
        insensitive = ins `elem` ["i", "I"]
        regex = mkRegexWithOpts match True insensitive
    e <- liftIO $ try (return $! subRegex regex string' replacement) :: Memory (Either SomeException String)
    case e of
        Right a -> return $ ChannelMsg a
        Left e -> return EmptyMsg
sed2 _ _ = return EmptyMsg

sed3 :: String -> Memory (Message String)
sed3 _ = return EmptyMsg

-- | Low level anime releases function, instead returning a list of strings.
anime' :: [String] -> String -> Memory [String]
anime' args str = do
    let (str', filters) = foldr' sepFilter ("", []) $ words str
        url = "https://tokyotosho.info/search.php?type=1&terms=" ++ urlEncode' str'
    html <- liftIO $ curlGetString' url []
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "a" (Just "http://www.w3.org/1999/xhtml") Nothing
        attrs = [Attr (QName "type" Nothing Nothing) "application/x-bittorrent"]
        elems = findElementsAttrs qname attrs elem'
        animes = map (strip . elemsText) elems
        animes' = filter (\x -> not . or $ map (`isInfixOf` x) filters) animes
        amount = if length args > 1 then (args !! 1) `safeRead` 10 else 10 :: Int
    verbosity <- asks (verbosityC . getConfig)
    liftIO . when (verbosity > 1) $ do
        putStrLn $ "Filters: %(f); str': %(s)" % [("f", join ", " filters), ("s", str')]
    return $ take amount animes'

-- | Anime releases function.
anime :: [String] -> String -> Memory (Message String)
anime args str = do
    animes <- anime' args str
    let animes' = map (replace "[" "[\ETX12" . replace "]" "\ETX]") animes
    return . ChannelMsg $ joinUntil ", " animes' 400

-- | Low level airing anime function, instead returning a list of Strings.
airing2 :: [String] -> String -> Memory [String]
airing2 args str = do
    let (str', filters) = foldr' sepFilter ("", []) $ words str
        amount = if length args > 1 then (args !! 1) `safeRead` 10 else 10 :: Int
        url = "http://www.mahou.org/Showtime/?o=ET"
        isSearch = not $ null str
    content <- liftIO $ curlGetString' url []
    let elem = fromMaybeElement $ parseXMLDoc content
        qTable = QName "table" Nothing Nothing
        aSummary = [Attr (QName "summary" Nothing Nothing) "Currently Airing"]
        table = findElementsAttrs qTable aSummary elem
        table2 = M.join $ fmap (findElementsIn qTable) table
        qTr = QName "tr" Nothing Nothing
        trs :: [Element]
        trs = drop 1 . M.join $ fmap (findElements qTr) table2
        tds :: [[String]]
        tds = fmap (fmap elemsText . contentsToEls . elContent) trs
        f (_ : series : season : station : company : time : eta : xs) acc =
            let seri = "\ETX12" ++ series ++ "\ETX"
                tim' = "\ETX08" ++ time ++ "\ETX"
                eta' = "(" ++ istrip eta ++ ")"
                nonSearch = unwords [seri, tim', eta']
                proSearch = unwords [ "\ETX12Series\ETX:", series
                                    , "\ETX12Season\ETX:", season
                                    , "\ETX12Time\ETX:", time
                                    , "\ETX12ETA\ETX:", istrip eta
                                    , "\ETX12Station\ETX:", station
                                    , "\ETX12Company\ETX:", company
                                    ]
            in if isSearch then
                let search = map (`isInfixOf` map toLower series)
                in if or $ search filters then acc
                    else if and . search . words $ map toLower str'
                        then proSearch : acc
                        else acc
               else nonSearch : acc
        backup = guard isSearch >> ["No results."]
    return . take amount $ foldr f [] tds <?> backup

-- | Airing anime function.
airing :: [String] -> String -> Memory (Message String)
airing args str = do
    airs <- airing2 args str
    if null airs
        then return EmptyMsg
        else return . ChannelMsg $ joinUntil ", " airs 400

-- TODO: Make this more advanced by printing whether the function is allowed
-- in this channel or not.
-- | Print information about the IRC commands the bot provides.
help :: [String] -> String -> Memory (Message String)
help _ str = do
    
    return . UserMsg $ getHelp str
  where getHelp s
            | s == "bind"       = "Bind, or >>. It executes the function on the left but ignores the output, and continues parsing the right."
            | s == "pipe"       = "Pipe, or ->. It appends the output of the function on the left into the function on the right."
            | s == "add"        = "Add, or ++. It appends the string of the output on the right to the output on the left."
            | s == "app"        = "Application operator, or $$. It appends the output of the function on the right into the function on the left. The opposite of ->."
            | s == ">"          = "Prints a string to the channel. Ex: .> hey"
            | s == "<"          = "Prints a string to the user. Ex: .< hey"
            | s == "^"          = "Gets a message from the channel history. Ex: .^ 3"
            | s == "$"          = "Gets a stored variable. Ex: .$:immutable myVariableName My variable message."
            | s == "ai"         = "Prints airtimes for this season's anime. Ex: .ai:1 yuru yuri"
            | s == "an"         = "Prints recent anime releases, can also be used to search. Ex: .an:3 Last Exile -Commie"
            | s == "lewd"       = "Prints a lewd message."
            | s == "ma"         = "Prints recent manga releases, can also be used to search. Ex: .ma:5 Banana no Nana"
            | s == "ra"         = "Prints a number from a range or a string choice split by `|'. Ex: .ra 1000; Ex: .ra Yes | No"
            | s == "sed"        = "Replaces text using regular expressions. Ex: .sed /probably should/should not/ You probably should help it."
            | s == "tr"         = "Translate a string. Ex: .tr:ja:en 日本語"
            | s == "we"         = "Prints the weather for a specified location. Ex: .we Tokyo"
            | s == "wiki"       = "Prints the introduction to an article. Ex: .wiki haskell programming language."
            | otherwise         = "Try `.help' and a value. Functions: >, <, ^, $, sed, ai, an, ma, ra, tr, we. Operators: bind, pipe, add, app. More: http://github.com/Shou-/KawaiiBot-hs#readme"

-- TODO: Use Text instead.
-- | Find the nth matching message from the channel chatlog. Where n is the
-- first colon argument, and a number.
--
-- If there is no searching string, match against everything, returning the
-- nth message from the whole chatlog.
findMsg :: [String] -> String -> Memory (Message String)
findMsg args str = do
    meta <- asks getMeta
    logsPath <- asks (logsPathC . getConfig)
    let path = logsPath ++ getServer meta ++ " " ++ getDestino meta
        (strs, filters) = foldr' sepFilterText ([], []) . T.words $ T.pack str
        searchF x = and $ map (`T.isInfixOf` snd x) strs
        filterF x = not . or $ map (`T.isInfixOf` snd x) filters
    r <- fmap (reverse . T.lines) . liftIO $ T.readFile path
    let history :: [([T.Text], T.Text)]
        history = do
            x <- r
            let msg = maybeRead (T.unpack x) :: Maybe ([T.Text], T.Text)
            guard $ msg /= Nothing
            guard $ if not $ null strs then searchF $ fromJust msg else True
            guard $ if not $ null filters then filterF $ fromJust msg else True
            return $ fromJust msg
        msg :: Message String
        msg = do
            let arg = fromJust $ listToMaybe args <|> Just "0"
                n = safeRead arg 0
            if length history > n
                then return . T.unpack . snd $ history !! n
                else EmptyMsg
    return msg

-- | Output lewd strings.
lewd2 :: [String] -> String -> Memory (Message String)
lewd2 _ _ = do
    meta <- asks getMeta
    lewdPath <- asks (lewdPathC . getConfig)
    lewds <- liftIO . fmap lines $ readFile lewdPath
    let parse :: [String] -> [(String, [String])]
        parse [] = []
        parse ([]:xs) = parse xs
        parse (x:xs) =
            let title = takeWhile (/= ':') x
                (start, rest) = inparse xs
            in (title, start) : parse rest
          where inparse :: [String] -> ([String], [String])
                inparse [] = ([], [])
                inparse ([]:xs) = inparse xs
                inparse ((' ':ys):xs) =
                    let x = dropWhile (== ' ') ys
                    in if null x
                        then inparse xs
                        else mapFst ([x] ++) $ inparse xs
                inparse xs = ([], xs)
        nonLewds = do
            (x, xs) <- parse lewds
            guard . not $ x `insEq` "lewds"
            guard $ length xs > 0
            return (x, xs)
    obj <- fmap (("nick", getUsernick meta) :) $ forM nonLewds $ \(x, xs) -> do
        n <- liftIO $ randomRIO (0, length xs - 1)
        return $ (x, xs !! n)
    let lewds' = fromJust $ lookup "lewds" (parse lewds) <|> Just []
    ln <- liftIO $ randomRIO (0, length lewds' - 1)
    if null lewds'
        then return EmptyMsg
        else return . UserMsg $ (lewds' !! ln) % obj

-- | Recent manga releases and searching function.
manga :: [String] -> String -> Memory (Message String)
manga cargs str = do
    let burl = "https://www.mangaupdates.com/releases.html?act=archive&search="
    html <- liftIO $ curlGetString' (burl ++ urlEncode' str) []
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "td" Nothing Nothing
        attrs = [Attr (QName "class" Nothing Nothing) "text pad"]
        elems = findElementsAttrs qname attrs elem'
        elems' = map elemsText elems
        -- Generate a list of colored strings
        genMangas :: [String] -> [String]
        genMangas (date:mangatitle:vol:chp:group:rest) =
            let mangaStr = unwords [ "\ETX12[" ++ strip group ++ "]\ETX"
                                   , mangatitle
                                   , "[Ch.\ETX08" ++ chp ++ "\ETX,"
                                   , "Vol.\ETX08" ++ vol ++ "\ETX]"
                                   , "(\ETX08" ++ date ++ "\ETX)"
                                   ]
            in mangaStr : genMangas rest
        genMangas _ = []
        maybeAmount =
            if length cargs > 1
                then fmap fst . listToMaybe . reads $ cargs !! 1
                else Just 10
        amount =
            if maybeAmount /= Nothing
                then fromJust maybeAmount 
                else 10
        mangas = take amount $ genMangas elems'
    return . ChannelMsg $ joinUntil ", " mangas 400

-- | Pick a random choice or number.
random :: [String] -> String -> Memory (Message String)
random args str
    | isNum str = do
        n <- liftIO (randomRIO (0, read str :: Integer))
        return . ChannelMsg $ show n
    | otherwise = do
        let choices = split "|" str
            len = length choices
        n <- liftIO (randomRIO (0, len - 1))
        if len > 0
            then return . ChannelMsg $ choices !! n
            else return EmptyMsg

-- | Website title function.
title :: String -> Memory (Message String)
title str = do
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
    response <- liftIO $ curlGetResponse str opts
    let headers = respHeaders response
        contentTypeResp = headers `tupleListGet` "Content-Type"
        contentResp = respBody response
    if (take 9 . strip) contentTypeResp == "text/html" then do
        let maybeElems = parseXMLDoc contentResp
            elems = if isNothing maybeElems
                        then Element (QName "" Nothing Nothing) [] [] Nothing
                        else fromJust maybeElems
            getTitle :: [Element]
            getTitle = filterElements ((== "title") . qName . elName) elems
            special = let f = fromJust $ specialElem <|> Just (\_ -> [])
                      in (`cutoff` 200) . (++ " ") . elemsText . head' $ f elems
            pagetitle = ((special ++ " ") ++) . unwords . map elemsText $ getTitle
        return . ChannelMsg . istrip . replace "\n" " " $ pagetitle
    else do
        return EmptyMsg
  where specialElem :: Maybe (Element -> [Element])
        specialElem
            | '#' `elem` str =
                let elemID = tail $ dropWhile (/= '#') str
                    attrs = [Attr (QName "id" Nothing Nothing) elemID]
                    f x = findElementsAttrs (QName "" Nothing Nothing) attrs x
                in Just f
            | let surl = split "/" str
              in surl !! 2 == "boards.4chan.org" && surl !! 4 == "res" =
                    Just $ findElements (QName "blockquote" Nothing Nothing)
            | otherwise = Nothing
        head' :: [Element] -> Element
        head' (x:_) = x
        head' _     = Element (QName "" Nothing Nothing) [] [] Nothing

-- | Check if website is up.
isup :: [String] -> String -> Memory (Message String)
isup _ str = do
    let url = "http://isup.me/" ++ removePrefixes ["https://", "http://"] str
    title' <- fmap fromMsg $ title url
    return . ChannelMsg . replace "Huh" "Nyaa" . unwords . take 4 . words $ title'

-- TODO: Replace with Microsoft Translate -- U FRUSTRATED U FRUSTRATED GOOGLE Y U SO MAAAAAAAAAAAAAAAAAAD
-- | Translate text to another language.
-- This does not work.
translate :: [String] -> String -> Memory (Message String)
translate args str = do
    msAppId <- asks (msAppIdC . getConfig)
    verbosity <- asks (verbosityC . getConfig)
    case msAppId of
        "" -> return EmptyMsg
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
                result' = fmap (lookup "TranslatedText" . fromJSObject . (!! 0)) result
                text = fromMaybeString $ fmap fromJSString (getOut result')
            return $ ChannelMsg text
  where getOut (Ok (Just (JSString a))) = Just a
        getOut _ = Nothing

-- TODO: Global variables
-- | Variable storing function.
variable2 :: [String] -> String -> Memory (Message String)
variable2 args str = do
    varPath <- asks (variablePathC . getConfig)
    nick <- asks (getUsernick . getMeta)
    dest <- asks (getDestino . getMeta)
    server <- asks (getServer . getMeta)
    let nick' = (!! 0) $ args <|> [nick]
        var :: Maybe (String, String, String)
        var = do
            let (varbegin, varcontent) = bisect (== '=') str
                (vartype, varname) =
                    let temp = bisect (== ' ') varbegin
                    in take 20 . head . words <$> if null $ snd temp 
                        then swap temp
                        else temp
            guard . not $ null varname || null varcontent
            return (vartype, varname, varcontent)
    case var of
      Nothing -> do -- read var
        liftIO $ putStrLn "Reading vars..."
        svars <- liftIO $ lines <$> readFile varPath
        let (isGlobal, varname) =
                let temp = bisect (== ' ') str
                    temp2 = if null $ snd temp then swap temp else temp
                in (fst temp2 == "Global", snd temp2)
            mvar :: Maybe String
            mvar = listToMaybe $ do
                v <- svars
                let mvars = maybeRead v
                guard $ mvars /= Nothing
                let var = fromJust mvars
                guard $ readableVar server dest nick' varname var
                guard $ if isGlobal then isGlobalVar var else True
                return $ varContents var
        case mvar of
            Just x -> do
                x' <- parser x
                return $ if x' == EmptyMsg
                    then ChannelMsg x
                    else x'
            Nothing -> do
                return $ EmptyMsg
      Just (vartype, varname, varcontent) -> do -- write var
        liftIO $ putStrLn "Writing var..."
        h <- liftIO $ openFile varPath ReadMode
        svars <- liftIO $ lines <$> hGetContents h
        let uvar = stringToVar vartype server dest nick' varname varcontent
            isGlobal = fromJust $ (return . (== "Global") . head $ words str)
                              <|> Just False
            isRemove = fromJust $ (return . (== "Remove") . head $ words str)
                              <|> Just False
            mvar :: [String]
            !mvar = do
                v <- svars
                let mvars = maybeRead v
                guard $ mvars /= Nothing
                let var = fromJust mvars
                guard . not $ writableVar server dest nick' varname var
                return $ show var
            vars = if isRemove then mvar else show uvar : mvar
        liftIO $ do
            hClose h
            writeFile varPath $ unlines vars
        return EmptyMsg

-- TODO: Store last location.
-- | Weather printing function.
weather :: [String] -> String -> Memory (Message String)
weather _ str = do
    let burl = "http://www.google.com/ig/api?weather="
    xml <- liftIO $ curlGetString' (burl ++ urlEncode' str) []
    let weatherElem = fromMaybeElement $ parseXMLDoc xml
        eCity = findElement (QName "city" Nothing Nothing) weatherElem
        eCurrent = findElements (QName "current_conditions" Nothing Nothing) weatherElem
        eForecast = findElements (QName "forecast_conditions" Nothing Nothing) weatherElem
        city = getData . fromMaybeElement $ eCity
        condition = map formatCondition . getConditionData $ eCurrent
        forecasts = map formatForecast . getConditionData $ eForecast
    return . ChannelMsg . join "; " $ condition ++ forecasts
  where getData element = fromMaybeString $ findAttr (QName "data" Nothing Nothing) element
        getConditionData elems = map (map getData . onlyElems . elContent) elems
        formatCondition [condition, tempF, tempC, humidity, _, wind] =
            concat ["\ETX12Today\ETX: ", tempF, "\176F / ", tempC, "\176C, ",
                    join ", " [condition, humidity, wind]]
        formatCondition _ = []
        formatForecast [day, _, _, _, condition] = "\ETX12" ++ day ++ "\ETX: " ++ condition
        formatForecast _ = []

-- | Wikipedia intro printing function.
wiki :: [String] -> String -> Memory (Message String)
wiki args s = do
    let burl = "https://en.wikipedia.org/w/index.php?search=%s&title=Special%3ASearch"
    html <- liftIO $ curlGetString' (burl % (replace "+" "%20" . urlEncode') s) []
    let xml = fromMaybeElement $ parseXMLDoc html
        qDiv = QName "div" (Just "http://www.w3.org/1999/xhtml") Nothing
        qMwcontent = [Attr (QName "id" Nothing Nothing) "mw-content-text"]
        element = fromMaybeElement $ findElementAttrs qDiv qMwcontent xml
        qPar = QName "p" (Just "http://www.w3.org/1999/xhtml") Nothing
        intro = findChild qPar element
        qMwsearch = [Attr (QName "class" Nothing Nothing) "mw-search-createlink"]
        search = findElementAttrs (QName "p" Nothing Nothing) qMwsearch element
        text = strip $ elemsText . fromMaybeElement $ search <|> intro
    if null text
        then return EmptyMsg
        else return . ChannelMsg . (`cutoff` 400) $ text

-------------------------------------------------------------------------------
-- Non-IRC bot functions / event functions ------------------------------------
-------------------------------------------------------------------------------

diff :: [String] -> Memory String
diff xs = do
    temp <- asks (getTemp . getMeta)
    let xs' = filter (not . (`elem` temp)) xs
    return $ joinUntil ", " xs' 400
