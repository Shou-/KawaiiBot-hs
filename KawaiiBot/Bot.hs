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

import qualified Data.Attoparsec.Text.Lazy as ATL
import Data.Char
import Data.List
import Data.Foldable (foldr')
import Data.Maybe
import Data.Ord
import Data.String.Utils
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Clock.POSIX
import Data.Tuple

import Debug.Trace

import Network.Curl
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser

import System.Directory
import System.IO
import System.Process (readProcessWithExitCode)
import System.Random (randomRIO)

import Text.Regex
import Text.JSON
import Text.XML.Light


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
interpret (Plain s) = prefixesC . getConfig <$> ask >>= run s
interpret (Add s f) = allowThen allowAdd $ do
    rs <- prefixesC . getConfig <$> ask >>= run s
    ex <- interpret f
    return $ rs `mergeMsg` ex
interpret (Bind s f) = allowThen allowBind $ interpret f
interpret (Pipe s f) = allowThen allowPipe $ do
    prefixesC . getConfig <$> ask >>= run s >>= interpret . deepPipe f
interpret (App s f) = allowThen allowApp $ do
    ex <- fmap fromMsg $ interpret f
    prefixesC . getConfig <$> ask >>= run (s ++ ex)

-- | Run an IRC bot function and return the string.
--
-- @
-- run \".> I like bananas.\"
-- @
run :: String -> [Char] -> Memory (Message String)
run x px =
    let (fcmd, fstring) = span (/= ' ') x
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
                lewd args string
        | cmd `isCmd` "sage" -> -- Sage a user
            allowThen allowSage $
                sage args string
        | cmd `isCmd` "ma" -> -- Recent manga releases
            allowThen allowManga $
                manga args string
        | cmd `isCmd` "ra" -> -- Random / choice
            allowThen allowRandom $
                random args string
        | cmd `isCmd` "sed" -> -- Regular expression replace
            allowThen allowSed $
                sed args string
        | cmd `isCmd` "tr" -> -- Translate 
            allowThen allowTranslate $
                translate args string
        | cmd `isCmd` "us" -> -- Userlist
            allowThen allowUserlist $
                userlist args string
        | cmd `isCmd` "yuri" -> -- Yuri battle
            allowThen allowYuri $
                yuri args string
        | cmd `isCmd` "we" -> -- Weather
            allowThen allowWeather $
                weather args string
        | cmd `isCmd` "wiki" -> -- Wikipedia
            allowThen allowWiki $
                wiki args string
        | cmd `isCmd` "e" -> -- Mueval
            allowThen allowMueval $
                heval args string
        | x `isCTCP` "VERSION" -> -- CTCP VERSION message
                return . UserMsg $ "VERSION " ++ botversion
        | otherwise -> return EmptyMsg
  where isCmd (x:xs) cmd = and [x `elem` px, xs == cmd]
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
sed :: [String] -> String -> Memory (Message String)
sed _ ('s':x:xs) = do
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
        insensitive = ins /= "i"
        regex = mkRegexWithOpts match False insensitive
    liftIO $ print insensitive
    e <- liftIO $ try (return $! subRegex regex string' replacement) :: Memory (Either SomeException String)
    case e of
        Right a -> return $ ChannelMsg a
        Left e -> return EmptyMsg
sed _ _ = return EmptyMsg

-- No more folding ``please!''
sed3 :: String -> Memory (Message String)
sed3 _ = return EmptyMsg

heval :: [String] -> String -> Memory (Message String)
heval args str = do
    debug <- asks (verbosityC . getConfig)
    -- Time: 2; Inferred type; Execute: `str'.
    let cargs = ["-t", "5", "-i", "-e", str]
    (ex, o, e) <- liftIO $ readProcessWithExitCode "mueval" cargs ""
    case lines o of
        [horig, htype, hout] -> do
            if any (`elem` args) ["t", "type", "T"]
                then return $ ChannelMsg $ cut $ unwords [ horig, "::", htype ]
                else return $ ChannelMsg $ cut $ hout
        xs -> do
            when (debug > 0) . liftIO . putStrLn $ "heval: " ++ show (lines o)
            return $ ChannelMsg $ cut $ intercalate "; " xs
  where cut = cutoff 400

-- | Low level anime releases function, instead returning a list of strings.
anime' :: [String] -> String -> Memory [String]
anime' args str = do
    let (str', filters) = foldr' sepFilter ("", []) $ words str
        burl = "http://www.nyaa.eu/?page=search&cats=1_37&filter=2&term="
    html <- liftIO $ curlGetString' (burl ++ urlEncode' str') []
    let elem' = fromMaybeElement $ parseXMLDoc html
        qname = QName "td" Nothing Nothing
        attrs = [Attr (QName "class" Nothing Nothing) "tlistname"]
        elems = findElementsAttrs qname attrs elem'
        animes = map (strip . elemsText) elems
        animes' = filter (\x -> not . or $ map (`isInfixOf` x) filters) animes
        amount = if length args > 1 then (args !! 1) `safeRead` 10 else 10
    verbosity <- asks (verbosityC . getConfig)
    liftIO . when (verbosity > 1) $ do
        putStrLn $ "Filters: %(f); str': %(s)" % [("f", join ", " filters), ("s", str')]
    return $ take amount animes'

-- | Anime releases function.
anime :: [String] -> String -> Memory (Message String)
anime args str = do
    animes <- anime' args str
    let animes' = map (replace "[" "[\ETX12" . replace "]" "\ETX]") animes
    return . ChannelMsg $ joinUntil 400 ", " animes'

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
        else return . ChannelMsg $ joinUntil 400 ", " airs

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
        searchF x = all (`T.isInfixOf` trd3 x) strs
        filterF x = not $ any (`T.isInfixOf` trd3 x) filters
    cs <- liftIO $ TL.readFile path
    let msgs :: [(T.Text, T.Text, T.Text)]
        msgs = do
            line <- reverse $ TL.lines cs
            let mresult = ATL.maybeResult . (flip ATL.parse) line $ do
                    time <- ATL.takeWhile1 (/= '\t')
                    ATL.char '\t'
                    nick <- ATL.takeWhile1 (/= '\t')
                    ATL.char '\t'
                    msg <- ATL.takeWhile1 (/= '\n')
                    return (time, nick, msg)
            guard $ isJust mresult
            return $ fromJust mresult
        searchh :: [(T.Text, T.Text, T.Text)]
        searchh = do
            msg <- msgs
            guard $ if not $ null strs then searchF msg else True
            guard $ if not $ null filters then filterF msg else True
            return msg
        msg :: Message String
        msg = do
            let arg = fromJust $ listToMaybe args <|> Just "0"
                n = safeRead arg 0
            if length searchh > n
                then return . T.unpack . trd3 $ searchh !! n
                else EmptyMsg
    return msg

-- | Print userlist
userlist :: [String] -> String -> Memory (Message String)
userlist _ _ = asks $ ChannelMsg . unwords . getUserlist . getMeta

-- | Output lewd strings.
lewd :: [String] -> String -> Memory (Message String)
lewd args str = do
    meta <- asks getMeta
    lewdPath <- asks (lewdPathC . getConfig)
    lewds <- liftIO . fmap lines $ readFile lewdPath
    let nonLewds = do
            (x, xs) <- parseConf lewds
            guard . not $ x `insEq` "lewds"
            guard $ length xs > 0
            return (x, xs)
        n = safeRead str 1
        mnick = listToMaybe $ filter (not . null) args
        nick = fromJust $ mnick <?> Just (getUsernick meta)
    obj <- fmap (("nick", nick) :) $ forM nonLewds $ \(x, xs) -> do
        n <- liftIO $ randomRIO (0, length xs - 1)
        return $ (x, xs !! n)
    let lewds' = fromJust $ lookup "lewds" (parseConf lewds) <|> Just []
    ln <- liftIO $ randomRIO (0, length lewds' - 1)
    case () of
      _ | null lewds' -> return EmptyMsg
        | n > 1 -> do
            nlewd <- fmap fromMsg $ lewd args (show $ n - 1)
            return . UserMsg $ unwords [((lewds' !! ln) % obj), nlewd]
        | otherwise -> return . UserMsg $ (lewds' !! ln) % obj

-- Credit for this goes to whoever on Rizon created it.
-- | Sage a user.
sage :: [String] -> String -> Memory (Message String)
sage args str = do
    meta <- asks getMeta
    let server = getServer meta
        snick = getUsernick meta
        shost = getHostname meta
    sagepath <- asks (sagePathC . getConfig)
    msagerss <- fmap (map maybeRead . lines) . liftIO $ readFile $ sagepath ++ "rs"
    let cstr = strip str
        sagerss :: [Sages]
        sagerss = map fromJust $ filter (/= Nothing) msagerss
        f :: Sages -> Bool
        f (Sages s _) = s `insEq` server
        msagers :: Maybe Sages
        nsagerss :: [Sages]
        (msagers, nsagerss)  = getWithList f sagerss
        sagers :: Sages
        sagers = fromJust $ msagers <?> Just (Sages server [])
        msager :: Maybe (String, String, Int, Double)
        nsagers :: [(String, String, Int, Double)]
        (msager, nsagers) = getWithList (insEq cstr . fst4) $ getSagers sagers
        g x = snd4 x == shost || fst4 x `insEq` snick
        (smsager, snsagers) = getWithList g $ nsagers
        userlist :: [String]
        userlist = getUserlist meta
        mnick :: Maybe String
        mnick = fst $ getWithList (insEq cstr) userlist
        jnick = fromJust $ mnick <?> Just []
        (nick', host, ns, to) = fromJust $ msager <?> Just (jnick, "", 0, 0.0)
        (snick', shost', sns, sto) = fromJust $ smsager <?> Just (snick, "", 0, 0.0)
    time <- fmap realToFrac . liftIO $ getPOSIXTime
    let ntime = time + 30
    case mnick of
        _ | snick `insEq` cstr || host == shost ->
                return $ UserMsg "You can't sage yourself."
          | sto > time -> return $ UserMsg "You can't sage too frequently."
        Just nick -> do
            let ns' = ns + 1
                sager' = (nick', host, ns', to)
                ssager' = if null shost'
                    then (snick', shost, sns, ntime)
                    else (snick', shost', sns, ntime)
                sagers' = sager' : ssager' : snsagers
                sagerss' = Sages server sagers' : nsagerss
                isDoubles x = elem x $ takeWhile (<= x) $ do
                    x <- [0, 100 .. ]
                    y <- [11, 22 .. 99]
                    return $ x + y
                count = case ns' of
                    _ | isDoubles ns' -> unwords [ show ns' ++ " times."
                                                 , "Check the doubles on this cunt."
                                                 ]
                      | ns' >= 144 -> unwords [ show ns' ++ " times."
                                              , "Request gline immediately."
                                              ]
                      | ns' >= 72 -> unwords [ show ns' ++ " times."
                                             , "Fuck this guy."
                                             ]
                      | ns' >= 36 -> unwords [ show ns' ++ " times."
                                             , "Someone ban this guy."
                                             ]
                      | ns' >= 18 -> unwords [ show ns' ++ " times."
                                             , "What a faggot..."
                                             ]
                      | ns' >= 9 -> unwords [ show ns' ++ " times."
                                             , "Kick this guy already."
                                             ]
                      | ns' >= 3 -> unwords [ show ns' ++ " times."
                                            , "This guy is getting on my nerves..."
                                            ]
                      | ns' > 1 -> show ns' ++ " times. What a bro."
                      | ns' > 0 -> show ns' ++ " time. What a bro."
            liftIO $ safeWriteFile (sagepath ++ "rs") $ unlines $ map show sagerss'
            return $ ChannelMsg $ unwords [ nick'
                                          , "has been \US\ETX12SAGED\ETX\US"
                                          , count ]
        Nothing -> do
            return $ UserMsg $ cstr ++ " is not in the channel."

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
        amount = fromMaybe 10 maybeAmount
        mangas = take amount $ genMangas elems'
    return . ChannelMsg $ joinUntil 400 ", " mangas

-- | Pick a random choice or number.
random :: [String] -> String -> Memory (Message String)
random args str
    | isDigits str = do
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
    if "text/html" `isInfixOf` strip contentTypeResp then do
        let maybeElems = parseXMLDoc contentResp
            elems = if isNothing maybeElems
                        then Element (QName "" Nothing Nothing) [] [] Nothing
                        else fromJust maybeElems
            getTitle :: [Element]
            getTitle = filterElements ((== "title") . qName . elName) elems
            special = let f = fromJust $ specialElem <|> Just (\_ -> [])
                      in (cutoff 200) . (++ " ") . elemsText . head' $ f elems
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

-- U FRUSTRATED U FRUSTRATED GOOGLE Y U SO MAAAAAAAAAAAAAAAAAAD
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
                guard $ isJust mvars
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
                guard $ isJust mvars
                let var = fromJust mvars
                guard . not $ writableVar server dest nick' varname var
                return $ show var
            vars = if isRemove then mvar else show uvar : mvar
        liftIO $ do
            hClose h
            writeFile varPath $ unlines vars
        return EmptyMsg

variable3 :: [String] -> String -> Memory (Message String)
variable3 args str = return EmptyMsg

-- | Battle yuri function.
yuri :: [String] -> String -> Memory (Message String)
yuri args str = do
    debug <- asks (verbosityC . getConfig)
    meta <- asks getMeta
    let serverurl = getServer meta
        dest = getDestino meta
        userlist = getUserlist meta
        cstr = strip str
        margs = listToMaybe args
    yuripath <- asks $ yuriPathC . getConfig
    mphrases <- fmap maybeRead . liftIO . readFile $ yuripath ++ " actions"
    myuriss <- fmap (map maybeRead . lines) . liftIO . readFile $ yuripath ++ " yuris"
    let nick1 = getUsernick meta
        host1 = getHostname meta
        fighters = map strip $ split "<|>" str
        yuriphrases :: [YuriAction]
        yuriphrases = fromJust $ mphrases <?> Just []
        yuriss :: [Yuris]
        yuriss = map fromJust $ filter (/= Nothing) myuriss
        myuris :: Maybe Yuris
        nyuriss :: [Yuris]
        (myuris, nyuriss) = getWithList (isYuris serverurl dest) yuriss
        yuris :: Yuris
        yuris = fromJust $ myuris <?> Just (Yuris serverurl dest [])
        owners :: [YuriOwner]
        owners = yuriOwners yuris
        mowner :: Maybe YuriOwner
        nowners :: [YuriOwner]
        (mowner, nowners) = getWithList (isOwner nick1) $ owners
        owner :: YuriOwner
        owner = fromJust $ mowner <?> Just (YuriOwner nick1 host1 0 [])
        globalExists x =
            any (any (insEq x . yuriName) . ownerYuris) $ yuriOwners yuris
        ownerExists x =
            any ((insEq x) . yuriName) $ ownerYuris owner
    time <- liftIO $ fmap realToFrac getPOSIXTime
    let ntime = time + 9 * 60
        remtime = ceiling (ownerTime owner - time)
        (mintime, sectime) = remtime `divMod` 60
        timestr = case () of
          _ | mintime == 0 -> show sectime ++ " second(s)."
            | sectime == 0 -> show mintime ++ " minute(s)."
            | otherwise -> unwords [ show mintime
                                   , "minute(s) and"
                                   , show sectime
                                   , "second(s)." ]
        timeoutMsg = UserMsg $ unwords [ "Please wait"
                                       , timestr ]
    case margs of
      _ | any (== Nothing) myuriss -> do
            when (debug > 0) . liftIO $ do
                print $ length $ filter (== Nothing) myuriss
            return $ ChannelMsg $ "An error occurred (%s actions)" % yuripath
        | "fight" `maybeInsEq` margs -> case () of
          _ | time < ownerTime owner -> return timeoutMsg
            | length fighters < 2 -> do
                return $ UserMsg $ unwords [
                      "Incorrect syntax."
                    , ".yuri:fight [yuri name] <|> [opponent yuri]"
                    ]
            | not . all (globalExists . strip) $ fighters -> do
                let yuri1 = strip $ fighters !! 0
                    yuri2 = strip $ fighters !! 1
                case () of
                  _ | not $ globalExists yuri1 ->
                        return $ UserMsg $ yuri1 ++ " doesn't exist."
                    | not $ globalExists yuri2 ->
                        return $ UserMsg $ yuri2 ++ " doesn't exist."
                    | otherwise ->
                        return $ UserMsg "Everything exploded in your face."
            | length yuriphrases - 1 < 0 -> do
                when (debug > 0) . liftIO $ do
                    putStrLn $ unwords [ "There is most likely an error with"
                                       , "your `" ++ yuripath ++ " actions' file."
                                       ]
                return $ ChannelMsg "An error occurred (fight)."
            | otherwise -> do
                let yuri1n = strip $ fighters !! 0
                    yuri2n = strip $ fighters !! 1
                    oyuris = ownerYuris owner
                    (moyuri, onyuris) = getWithList (insEq yuri1n . yuriName) oyuris
                    oyuri = fromJust $ moyuri <?> Just (Yuri [] "" Nothing (0,0,0))
                    (meowner, enowner) = getWithList (any (insEq yuri2n . yuriName) . ownerYuris) owners
                    eowner = fromJust $ meowner <?> Just (YuriOwner [] [] 0 [])
                    enick = ownerName eowner
                    ehost = ownerHost eowner
                    etime = ownerTime eowner
                    eyuris = ownerYuris eowner
                    (meyuri, enyuris) = getWithList (insEq yuri2n . yuriName) eyuris
                    eyuri = fromJust $ meyuri <?> Just (Yuri [] "" Nothing (0,0,0))
                    woystats = yuriStats oyuri `applyWep` yuriWeapon oyuri
                    weystats = yuriStats eyuri `applyWep` yuriWeapon eyuri
                    (moe1, lewd1, str1) =
                        woystats `statsMinus` weystats
                    (moe2, lewd2, str2) =
                        weystats `statsMinus` woystats
                    phrases = filter (not . (`insElem` ["steal", "esteal"]) . actionName) yuriphrases
                    omoephs =
                        take moe1 . safeCycle $ filter (insEq "moe" . actionName) phrases
                    olewdphs =
                        take lewd1 . safeCycle $ filter (insEq "lewd" . actionName) phrases
                    ostrphs =
                        take str1 . safeCycle $ filter (insEq "str" . actionName) phrases
                    emoephs =
                        take moe2 . safeCycle $ filter (insEq "emoe" . actionName) phrases
                    elewdphs =
                        take lewd2 . safeCycle $ filter (insEq "elewd" . actionName) phrases
                    estrphs =
                        take str2 . safeCycle $ filter (insEq "estr" . actionName) phrases
                    newphs = concat [ omoephs
                                    , olewdphs
                                    , ostrphs
                                    , emoephs
                                    , elewdphs
                                    , estrphs
                                    , phrases
                                    ]
                    ostealphs =
                        let f | length oyuris < 6 =
                                (insEq "steal" . actionName)
                              | otherwise = \_ -> False
                            amount = (moe1 + lewd1 + str1) `div` 4
                        in take amount . safeCycle $ filter f yuriphrases
                    estealphs =
                        let f | length eyuris < 6 =
                                (insEq "esteal" . actionName)
                              | otherwise = \_ -> False
                            amount = (moe2 + lewd2 + str2) `div` 4
                        in take amount . safeCycle $ filter f yuriphrases
                    stealphs =
                        let f | length oyuris < 6 && length eyuris < 6 =
                                ((`insElem` ["steal", "esteal"]) . actionName)
                              | length oyuris < 6 =
                                (insEq "steal" . actionName)
                              | length eyuris < 6 =
                                (insEq "esteal" . actionName)
                              | otherwise = (\_ -> False)
                        in filter f yuriphrases
                    finalphs = concat [ostealphs, estealphs, stealphs, newphs]
                    stats = totalStats oyuri - totalStats eyuri
                n <- liftIO $ randomRIO (0, length finalphs - 1)
                when (debug > 1) . liftIO $ do
                    putStrLn $ unlines [ unwords [ "Moe phs:"
                                                 , show (length omoephs)
                                                 , "+"
                                                 , show (length emoephs)
                                                 , "/"
                                                 , show (length finalphs)
                                                 ]
                                       , unwords [ "Lewd phs:"
                                                 , show (length olewdphs)
                                                 , "+"
                                                 , show (length elewdphs)
                                                 , "/"
                                                 , show (length finalphs)
                                                 ]
                                       , unwords [ "Str phs:"
                                                 , show (length ostrphs)
                                                 , "+"
                                                 , show (length estrphs)
                                                 , "/"
                                                 , show (length finalphs)
                                                 ]
                                       , unwords [ "Steal phs:"
                                                 , show (length stealphs)
                                                 , "+"
                                                 , show (length ostealphs)
                                                 , "+"
                                                 , show (length estealphs)
                                                 , "/"
                                                 , show (length finalphs)
                                                 ]
                                       , unwords [ "Yuri1 vs Yuri2:"
                                                 , show woystats
                                                 , "vs"
                                                 , show weystats
                                                 ]
                                       ]
                case () of
                  _ | yuriName oyuri == [] -> do
                        return $ UserMsg $ unwords [ yuriName oyuri
                                                   , "isn't owned by you."
                                                   ]
                    -- what the hell is this
                    | n < 0 || n >= length finalphs -> do
                        liftIO $ do
                            putStrLn $ "N: " ++ show n
                            putStrLn $ "Len phs: " ++ show (length finalphs)
                        return $ EmptyMsg
                    --
                    | stats `notElem` [-10 .. 10] -> do
                        let status | stats > 10 = "too high"
                                   | stats < (-10) = "too low"
                            name = yuriName eyuri
                        return $ UserMsg $ unwords [ yuriName oyuri ++ "'s"
                                                   , "stats are %s" % status
                                                   , "to fight %s." % name
                                                   , "Max 10 stat difference."
                                                   ]
                    | otherwise -> do
                        let mphrase = finalphs !! n
                            msgs = actionMsgs mphrase
                            yuri1s = yuriName oyuri
                            yuri2s = yuriName eyuri
                            yimg1 = " <" >|< yuriImage oyuri >|< ">"
                            yimg2 = " <" >|< yuriImage eyuri >|< ">"
                            lewdnick = if isPrefixOf "E" $ actionName mphrase
                                then yuri2s
                                else yuri1s
                        n' <- liftIO $ randomRIO (0, length msgs - 1)
                        lewdmsg <- fmap fromMsg $ lewd [lewdnick] ""
                        let stats1 = actionYuri1 mphrase
                            stats2 = actionYuri2 mphrase
                            obj = [ ("yuri1", yuri1s)
                                  , ("yuri2", yuri2s)
                                  , ("nick1", nick1)
                                  , ("nick2", enick)
                                  , ("img1", yimg1)
                                  , ("img2", yimg2)
                                  , ("lewd", lewdmsg)
                                  ]
                            msg = (msgs !! n') % obj
                            oyuri' = applyStats stats1 oyuri
                            eyuri' = applyStats stats2 eyuri
                            oyuris' = case actionName mphrase of
                                "Steal" -> eyuri' : oyuri' : onyuris
                                "ESteal" -> onyuris
                                _ -> oyuri' : onyuris
                            eyuris' = case actionName mphrase of
                                "Steal" -> enyuris
                                "ESteal" -> oyuri' : eyuri' : enyuris
                                _ -> eyuri' : enyuris
                            owner' = YuriOwner nick1 host1 ntime oyuris'
                            eowner' = YuriOwner enick ehost etime eyuris'
                            nowners' = filter ((/= enick) . ownerName) nowners
                            owners' = owner' : eowner' : nowners'
                            yuris' = addOwners yuris owners'
                            yuriss' = yuris' : nyuriss
                        case () of
                          _ | enick `insEq` nick1 -> do
                                return $ UserMsg "You can't fight your own yuri!"
                            | otherwise -> do
                                writeYuriss yuriss'
                                return $ ChannelMsg msg
        | "spawn" `maybeInsEq` margs -> case () of
          _ | time < ownerTime owner -> return timeoutMsg
            | otherwise -> do
                let poorest = sortBy (comparing (length . ownerYuris)) owners
                    poorest' = let f g x y = (g x, g y)
                                   g x y = uncurry (<=) $ f (length . ownerYuris) x y
                               in map ownerName $ takeWhileOrd g poorest
                    userlist' = filter (`elem` userlist) poorest' <?> userlist
                n <- liftIO $ randomRIO (0, length userlist' - 1)
                let rnick = userlist' !! n
                    (mo', no') = getWithList (isOwner rnick) owners
                    (_, no'') = getWithList (isOwner nick1) no'
                    o' = fromJust $ mo' <?> Just (YuriOwner rnick [] 0 [])
                    (name' : img : _) = (fighters <?> [cstr, ""]) ++ [""]
                    yurixs = Yuri name' img Nothing (0, 0, 0) : ownerYuris o'
                    otime = ownerTime o'
                    ohost = ownerHost o'
                    owner' = YuriOwner rnick ohost otime yurixs
                    myowner = ownerModTime owner ntime
                    twowners = if rnick == nick1
                        then [YuriOwner rnick ohost ntime yurixs]
                        else [owner', myowner]
                    yuris' = addOwners yuris $ twowners ++ no''
                    yuriss = yuris' : nyuriss
                case () of
                  _ | globalExists name' ->
                        return $ UserMsg $ name' ++ " already exists."
                    | null img -> do
                        return $ UserMsg $ unwords [ "Image URL required."
                                                   , ":spawn yuri name"
                                                   , "<|>"
                                                   , "image URL"
                                                   ]
                    | length name' > 60 -> do
                        return $ UserMsg "Name is too long. 60 chars is max."
                    | length img > 37 -> do
                        return $ UserMsg "Image URL too long. 37 chars is max."
                    | length yurixs <= 6 -> do
                        writeYuriss yuriss
                        return $ ChannelMsg $ unwords [ name'
                                                      , "<" ++ img ++ ">"
                                                      , "was given to"
                                                      , rnick ++ "." ]
                    | length yurixs >= 6 -> do
                        return $ UserMsg $ unwords [ rnick
                                                   , "has too many yuri."
                                                   , "Six is max."
                                                   ]
                    | otherwise -> return $ ChannelMsg $ "An error occurred (spawn)."
        | "find" `maybeInsEq` margs -> do
        let matcher f xs = do
                owner' <- xs
                let yuris = filter f $ ownerYuris owner'
                    nick' = ownerName owner'
                guard $ not $ null yuris
                let mkMatch (Yuri n i w s) = unwords [n, s `showApplyWep` w]
                    yuris' = intercalate ", " $ map mkMatch yuris
                return $ unwords [nick', "has", yuris']
            ematches = matcher (insEq cstr . yuriName) owners
            pmatches = matcher (insIsInfixOf cstr . yuriName) owners
            matches = joinUntil 400 "; " . (ematches ++) $ do
                x <- pmatches
                guard $ x `notElem` ematches
                return x
        case matches of
            [] -> return $ UserMsg $ "No matches against `" ++ cstr ++ "'."
            _ -> return $ ChannelMsg $ matches
        | "list" `maybeInsEq` margs -> do
        let nick' = if null cstr then nick1 else cstr
            mowner' = fst . getWithList ((insEq nick') . ownerName) $ owners
            noyuris = if nick' `insEq` nick1
                then "You don't have any Yuri!"
                else nick' ++ " doesn't have any Yuri!"
        case mowner' of
            Just owner' -> do
                let stats :: Yuri -> String
                    stats hyu@(Yuri n i w s) = unwords [ n, showWep hyu]
                    yurisstr = intercalate ", " $ map stats $ ownerYuris owner'
                if null yurisstr then do
                    return $ UserMsg $ noyuris
                else return $ ChannelMsg yurisstr
            Nothing -> return $ UserMsg $ noyuris
        | "drop" `maybeInsEq` margs -> case () of
          _ | not $ ownerExists cstr -> do
                return $ UserMsg $ cstr ++ " doesn't exist."
            | otherwise -> do
                let yurixs = filter (not . insEq cstr . yuriName) $ ownerYuris owner
                    owner' = YuriOwner nick1 host1 ntime yurixs
                    yuris' = addOwners yuris $ owner' : nowners
                    yuriss = yuris' : nyuriss
                writeYuriss yuriss
                return $ ChannelMsg $ unwords [ cstr
                                              , "was dropped by"
                                              , nick1 ++ "." ]
        | "rank" `maybeInsEq` margs -> do
            let mn = maybeRead . dropWhile (not . isDigit) $ cstr :: Maybe Int
                jn = fromJust $ mn <?> Just 0
                moder [] = (>= jn)
                moder (x:_) | x == '>' = (> jn)
                            | x == '<' = (< jn)
                            | x == '=' = (== jn)
                            | x == '~' = \y -> y > jn - 10 && y < jn + 10
                            | otherwise = (>= jn)
                mf = moder $ take 1 cstr
                yurixs = concatMap ownerYuris owners
                yurixs' = filter (mf . totalStats) $ case () of
                  _ | cstr `insElem` ["moe", "fst"] ->
                        sortBy (comparing (oneStat fst3)) yurixs
                    | cstr `insElem` ["lewd", "snd", "lewdness"] ->
                        sortBy (comparing (oneStat snd3)) yurixs
                    | cstr `insElem` ["str", "trd", "strength"] ->
                        sortBy (comparing (oneStat trd3)) yurixs
                    | cstr `insElem` ["min", "weak"] ->
                        reverse $ sortBy (comparing totalStats) yurixs
                    | otherwise ->
                        sortBy (comparing totalStats) yurixs
                yren y = unwords [ yuriName y
                                 , yuriStats y `showApplyWep` yuriWeapon y
                                 ]
                yurixstr =
                    joinUntil 400 ", " . take 15 . reverse $ map yren yurixs'
            return $ ChannelMsg yurixstr
        | "wgive" `maybeInsEq` margs -> do
            let (name : wep : _) = (fighters <?> [cstr, ""]) ++ [""]
                oyuris = ownerYuris owner
                (myuri, nyuris) = getWithList (insEq name . yuriName) oyuris
                fyuri = listToMaybe $ do
                    o <- owners
                    let mm = fst $ getWithList (insEq name . yuriName) $ ownerYuris o
                    guard $ isJust mm
                    return $ fromJust mm
                yuri = fromJust $ myuri <?> fyuri <?> Just (Yuri "" "" Nothing (0,0,0))
            amn <- liftIO $ randomRIO (0, 2)
            let (rmoe : rlewd : rstr : _) = case () of
                  _ | isNothing (yuriWeapon yuri) || not (null wep) ->
                        take 3 . drop amn $ cycle [0, 0, 1]
                    | otherwise -> getBonusStat $ yuriWeapon yuri
                chance = 2 ^ getWeaponStat (yuriWeapon yuri)
            mwn <- liftIO $ randomRIO (1, chance)
            let isWin = 0 == [0 .. chance - 1] !! (mwn - 1)
                newstats =
                    let (x, y, z) = getWepStats $ yuriWeapon yuri
                    in (x + rmoe, y + rlewd, z + rstr)
                wep' = wep <?> wepName (yuriWeapon yuri)
                weapon = if isWin
                    then Just (wep', newstats)
                    else Just (wep', getWepStats $ yuriWeapon yuri)
            case () of
              _ | time < ownerTime owner -> do
                    return timeoutMsg
                | null $ yuriName yuri -> do
                    return $ UserMsg $ name ++ " doesn't exist."
                | not $ ownerExists name -> do
                    return $ UserMsg $ "You don't own " ++ name ++ "."
                | otherwise -> do
                    let (Yuri name' img' _ stats') = yuri
                        yuri' :: Yuri
                        yuri' = Yuri name' img' weapon stats'
                        yurixs :: [Yuri]
                        yurixs = yuri' : nyuris
                        owner' :: YuriOwner
                        owner' = YuriOwner nick1 host1 ntime yurixs
                        owners' :: [YuriOwner]
                        owners' = owner' : nowners
                        yuris' :: Yuris
                        yuris' = addOwners yuris $ owners'
                        yuriss :: [Yuris]
                        yuriss = yuris' : nyuriss
                        sta = if isWin then "succeeded" else "failed"
                        msg = if null wep
                            then "Weapon upgrade %s!" % sta
                            else "%s was given to %s." % [wep', yuriName yuri']
                    liftIO $ do
                        putStrLn $ concat ["Chance: 1 / ", show chance]
                    writeYuriss yuriss
                    return $ ChannelMsg $ unwords [ "Weapon upgrade %s!" % sta
                                                  , yuriName yuri'
                                                  , showWep yuri'
                                                  ]
        | "look" `maybeInsEq` margs -> do
            let (name : img : _) = (fighters <?> [cstr, ""]) ++ [""]
                oyuris = ownerYuris owner
                (myuri, nyuris) = getWithList (insEq name . yuriName) oyuris
                fyuri = listToMaybe $ do
                    o <- owners
                    let mm = fst $ getWithList (insEq name . yuriName) $ ownerYuris o
                    guard $ isJust mm
                    return $ fromJust mm
                yuri = fromJust $ myuri <?> fyuri <?> Just (Yuri "" "" Nothing (0,0,0))
            case () of
              _ | null $ yuriName yuri -> do
                    return $ UserMsg $ name ++ " doesn't exist."
                | length img > 37 -> do
                    return $ UserMsg "Image URL is too long. 37 chars is max."
                | null img -> do
                    return $ ChannelMsg $ unwords [
                          yuriName yuri
                        , show (yuriStats yuri)
                        , yuriImage yuri
                        ]
                | not $ ownerExists name -> do
                    return $ UserMsg $ "You don't own " ++ name ++ "."
                | otherwise -> do
                    let (Yuri name' _ wep' stats') = yuri
                        yuri' :: Yuri
                        yuri' = Yuri name' img wep' stats'
                        yurixs :: [Yuri]
                        yurixs = yuri' : nyuris
                        owner' :: YuriOwner
                        owner' = YuriOwner nick1 host1 (ownerTime owner) yurixs
                        owners' :: [YuriOwner]
                        owners' = owner' : nowners
                        yuris' :: Yuris
                        yuris' = addOwners yuris $ owners'
                        yuriss :: [Yuris]
                        yuriss = yuris' : nyuriss
                    writeYuriss yuriss
                    return EmptyMsg
      _ -> return $ UserMsg $ unwords [
              "Please give an argument like:"
            , ":fight yuri name <|> opponent yuri,"
            , ":spawn yuri name <|> image URL,"
            , ":find yuri name,"
            , ":list [IRC nick],"
            , ":drop yuri name,"
            , ":rank [stat name],"
            , ":wgive yuri name [<|> weapon name],"
            , ":look yuri name [<|> Yuri image URL],"
            ]

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
        else return . ChannelMsg . (cutoff 400) $ text

-------------------------------------------------------------------------------
-- Non-IRC bot functions / event functions ------------------------------------
-------------------------------------------------------------------------------

diff :: [String] -> Memory String
diff xs = do
    temp <- asks (getUserlist . getMeta)
    let xs' = filter (not . (`elem` temp)) xs
    return $ joinUntil 400 ", " xs'
