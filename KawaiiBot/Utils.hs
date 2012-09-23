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

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

module KawaiiBot.Utils where


import KawaiiBot.Types

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Applicative
import Control.Concurrent
import Control.Exception
import qualified Control.Monad as M
import Control.Monad.Reader

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.Char (toUpper, toLower, isDigit)
import Data.Maybe (fromJust, listToMaybe)
import Data.String.Utils (split, replace)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Network.CGI (urlEncode, urlDecode)
import Network.Curl
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser

import System.Directory
import System.IO

import Text.XML.Light


botversion = "KawaiiBot 0.1.7"

-------------------------------------------------------------------------------
-- ** Message utils
-------------------------------------------------------------------------------

-- | From message to string
fromMsg :: Message String -> String
fromMsg (ChannelMsg x) = x
fromMsg (UserMsg x) = x
fromMsg EmptyMsg = []

-- | Message String to @(String -> Message)@
msgtype :: Message String -> (String -> Message String)
msgtype (UserMsg _) = UserMsg
msgType _ = ChannelMsg

-- | Is Message a `ChannelMsg'?
isChannelMsg :: Message String -> Bool
isChannelMsg (ChannelMsg _) = True
isChannelMsg _ = False

-- | Prepend the first Message to the second.
mergeMsg :: Message String -> Message String -> Message String
mergeMsg (UserMsg a) b = fmap ((a ++) . (" " ++)) b
mergeMsg (ChannelMsg a) b = fmap ((a ++) . (" " ++)) b
mergeMsg EmptyMsg b = b

-------------------------------------------------------------------------------
-- ** Server utils
-------------------------------------------------------------------------------

-- | Get a server from a list of servers by the server's URL
getByServerURL :: String -> [Server] -> Maybe Server
getByServerURL s xs = foldr f Nothing xs
  where f = (\x acc -> if serverURL x == s then Just x else acc)

-- what
findServer :: [Server] -> String -> Maybe Server
findServer [] _ = Nothing
findServer (server@(Server _ url _ _ _ _ _ _):xs) s = if s == url
    then Just server
    else findServer xs s

injectServerUserlist :: [Server] -- original servers
                     -> String -- server url
                     -> String -- channel
                     -> [String] -- userlist
                     -> [Server] -- modified servers
injectServerUserlist xs s c us =
    let mserver = xs `findServer` s
        servers = do
            x <- xs
            guard $ serverURL x /= s
            return x
    in case mserver of
        Just (Server po se ch ni ns ac af ms) ->
            let im = do
                    m <- ms
                    guard $ getDestino m /= c
                    return m
                mm = listToMaybe $ do
                    m <- ms
                    guard $ getDestino m == c
                    return m
            in case mm of
                Just (Meta des nic nam hos cha ser use) ->
                    let meta = Meta des nic nam hos cha ser us
                        metas = meta : im
                        server = Server po se ch ni ns ac af metas
                    in server : servers
                -- How to handle new channels?
                Nothing ->
                    let meta = Meta c [] [] [] [] s us
                        metas = meta : im
                        server = Server po se ch ni ns ac af metas
                    in server : servers
        -- How to handle new servers?
        Nothing -> xs

-------------------------------------------------------------------------------
-- ** Tuple utils
-------------------------------------------------------------------------------

tupleListGet :: [(String, [a])] -> String -> [a]
tupleListGet tupleList str = foldr (\(x, y) acc -> if x == str then y else acc) [] tupleList

tupleInject :: (String, a) -> [(String, a)] -> [(String, a)]
tupleInject var@(name, _) vars =
    let f var'@(name', _) (status', acc) =
            if name == name'
                then (True, var : acc)
                else (status', var' : acc)
        (status, list) = foldr f (False, []) vars
    in if status then list else var : list

insLookup :: String -> [(String, b)] -> Maybe b
insLookup _ [] = Nothing
insLookup s ((s',b):xs) = if s `insEq` s' then Just b else insLookup s xs

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

trd4 :: (a, b, c, d) -> c
trd4 (_, _, x, _) = x

frd4 :: (a, b, c, d) -> d
frd4 (_, _, _, x) = x

-------------------------------------------------------------------------------
-- ** String utils
-------------------------------------------------------------------------------

-- | Split a String several times with the Char list
splits :: [Char] -> String -> [String]
splits (x:xs) y = split [x] $ replaces xs [x] y
  where replaces (x':xs') y' s = replace [x'] y' (replaces xs' y' s)
        replaces [] _ s     = s
splits [] _ = []

-- | Strip that removes unnecessary infix whitespace
istrip :: String -> String
istrip (x:xs) | x /= ' '  = x : istrip' xs
              | otherwise = istrip' $ dropWhile (== ' ') xs
  where istrip' (' ':xs') = istrip'' xs'
        istrip' (x':xs')   = x' : istrip' xs'
        istrip' []       = []
        istrip'' (' ':xs')  = istrip'' xs'
        istrip'' (x':xs')    = ' ' : x' : istrip' xs'
        istrip'' []         = []
istrip _ = []

-- can't you just use the list monad for this
-- | A list of prefixes to remove from a string.
removePrefixes :: Eq a => [[a]] -> [a] -> [a]
removePrefixes [] str = str
removePrefixes (x:xs) str =
    let f n [] = (n == length x, [])
        f n (x':xs') =
            let rest = drop n x
            in if length rest > 0
                then if x' == head (drop n x)
                    then f (n+1) xs'
                    else (n == length x, xs')
                else (n == length x, x':xs')
        (b, str') = f 0 str
    in if b
        then str'
        else removePrefixes xs str

sepFilter :: String -> (String, [String]) -> (String, [String])
sepFilter x (stracc, filacc) = if negateWord x
    then (stracc, tail x : filacc)
    else (' ' : x ++ stracc, filacc)
  where negateWord :: String -> Bool
        negateWord ('-':_) = True
        negateWord _       = False

sepFilterText :: T.Text -> ([T.Text], [T.Text]) -> ([T.Text], [T.Text])
sepFilterText x (stracc, filacc) = if negateWord x
    then (stracc, T.tail x : filacc)
    else (x : stracc, filacc)
  where negateWord :: T.Text -> Bool
        negateWord = T.isPrefixOf (T.pack "-")

sepFilterLText :: TL.Text -> ([TL.Text], [TL.Text]) -> ([TL.Text], [TL.Text])
sepFilterLText x (stracc, filacc) = if negateWord x
    then (stracc, TL.tail x : filacc)
    else (x : stracc, filacc)
  where negateWord :: TL.Text -> Bool
        negateWord = TL.isPrefixOf (TL.pack "-")

unlines' :: [String] -> String
unlines' (x:[]) = x
unlines' xs     = unlines xs

-- | Take until Int then add an ellipsis
cutoff :: Int -> String -> String
cutoff n s = if length s > n then (take n s) ++ "…" else s

-- | Join until length String >= Int
joinUntil :: Int -> String -> [String] -> String
joinUntil n str strs =
    let str' = snd $ foldl (\(status, acc) x ->
            let acc' = acc ++ x ++ str
            in if status && length acc' <= n
                    then (True, acc')
                    else (False, acc)) (True, "") strs
    in take (length str' - length str) str'

-------------------------------------------------------------------------------
-- ** List utils
-------------------------------------------------------------------------------

-- | Break, except the break match goes into fst of the tuple instead
breakLate :: (a -> Bool) -> [a] -> ([a],  [a])
breakLate f xs =
    let g t@(_,[]) = t
        g (x,(y:ys)) = (x ++ [y], ys)
    in g $ break f xs

-- | Break without preserving the matching `a'
bisect :: (a -> Bool) -> [a] -> ([a], [a])
bisect f xs = fmap (drop 1) $ break f xs

-- | Take a matching element from the list and return the rest of the list.
getWithList :: (a -> Bool) -> [a] -> (Maybe a, [a])
getWithList _ [] = (Nothing, [])
getWithList f (x:xs) | f x = (Just x, xs)
                     | otherwise = let (m, xs') = getWithList f xs
                                   in (m, x : xs')

safeCycle :: [a] -> [a]
safeCycle [] = []
safeCycle xs = cycle xs

-------------------------------------------------------------------------------
-- ** Variable utils
-------------------------------------------------------------------------------

-- Server, Channel, Nick, Varname, Variable
-- | Is the `Variable' readable by the user?
readableVar :: String -> String -> String -> String -> Variable -> Bool
readableVar  se ch ni na (Personal se' ch' ni' na' _) =
    se == se' && ch `insEq` ch' && ni `insEq` ni' && na == na'
readableVar  se ch ni na (Reminder se' ch' ni' na' _) =
    se == se' && ch `insEq` ch' && ni `insEq` ni' && na == na'
readableVar se ch _ na (Immutable se' ch' _ na' _) =
    se == se' && ch `insEq` ch' && na == na'
readableVar se ch _ na (Normal se' ch' _ na' _) =
    se == se' && ch `insEq` ch' && na == na'
readableVar _ _ _ _ _ = False

-- | Is the Variable `Global'?
isGlobalVar :: Variable -> Bool
isGlobalVar (Global {}) = True
isGlobalVar _ = False

-- | Is the Variable a readable `Reminder'?
readableReminder :: String -> String -> String -> Variable -> Bool
readableReminder se ch ni (Reminder se' ch' ni' _ _) =
    se == se' && ch == ch' && ni == ni'
readableReminder _ _ _ _ = False

-- | Is the Variable writable by the user?
writableVar :: String -> String -> String -> String -> Variable -> Bool
writableVar se ch ni na (Personal se' ch' ni' na' _) =
    se == se' && ch `insEq` ch' && ni `insEq` ni' && na == na'
writableVar se ch ni na (Reminder se' ch' ni' na' _) =
    se == se' && ch `insEq` ch' && ni `insEq` ni' && na == na'
writableVar se ch ni na (Immutable se' ch' ni' na' _) =
    se == se' && ch `insEq` ch' && ni `insEq` ni' && na == na'
writableVar se ch _ na (Normal se' ch' _ na' _) =
    se == se' && ch `insEq` ch' && na == na'
writableVar _ _ _ _ _ = False

-- | Variable content data
varContents :: Variable -> String
varContents (Personal _ _ _ _ c) = c
varContents (Reminder _ _ _ _ c) = c
varContents (Immutable _ _ _ _ c) = c
varContents (Normal _ _ _ _ c) = c
varContents (Global _ _ c) = c

-- | Variable name data
varName :: Variable -> String
varName (Personal _ _ _ na _) = na
varName (Reminder _ _ _ na _) = na
varName (Immutable _ _ _ na _) = na
varName (Normal _ _ _ na _) = na
varName (Global _ na _) = na

-- | String to Variable
stringToVar :: String -- ^ Variable constructor function
             -> String -- ^ Server
             -> String -- ^ Channel
             -> String -- ^ Nick
             -> String -- ^ Variable name
             -> String -- ^ Variable content
             -> Variable
stringToVar f se ch ni na co
    | f `insEq` "Immutable" = Immutable se ch ni na co
    | f `insEq` "Reminder" = Reminder se ch ni na co
    | f `insEq` "Personal" = Personal se ch ni na co
    | f `insEq` "Global" = Global ni na co
    | otherwise = Normal se ch ni na co

-------------------------------------------------------------------------------
-- ** HTTP utils
-------------------------------------------------------------------------------

httpGetResponse :: MonadIO m => String -> m (String, [(String, String)], String, String)
httpGetResponse url = liftIO $ withManager $ \man -> do
    initReq <- parseUrl url
    let req = initReq { requestHeaders = (htitle, useragent)
                                         : requestHeaders initReq
                      }
    r <- httpLbs req man
    let b = responseBody r
        s = responseStatus r
        v = responseVersion r
        h = responseHeaders r
    return $ ( BU.toString $ B.concat $ BL.toChunks b
             , flip map h $ \(k, v) ->
                (BU.toString $ CI.original k, BU.toString v)
             , show s
             , show v
             )
  where
    htitle = "User-Agent"
    useragent = "Mozilla/5.0 (X11; Linux x86_64; rv:10.0.2) Gecko/20100101 Firefox/10.0.2"

httpGetString :: MonadIO m => String -> m String
httpGetString url = liftIO $ withManager $ \man -> do
    (str, _, _, _) <- httpGetResponse url
    if length str > 10 ^ 6 * 2
        then return ""
        else return str
  where
    htitle = "User-Agent"
    useragent = "Mozilla/5.0 (X11; Linux x86_64; rv:10.0.2) Gecko/20100101 Firefox/10.0.2"

-- | Encode a URL
urlEncode' :: String -> String
urlEncode' = urlEncode . encodeString

-- | Decode an encoded URL
urlDecode' :: String -> String
urlDecode' = decodeString . urlDecode

-------------------------------------------------------------------------------
-- ** CMemory/Memory utils
-------------------------------------------------------------------------------

io :: MonadIO m => IO a -> m a
io = liftIO

getcConfig :: CMemory Config
getcConfig = do
    mvs <- asks getcMVars
    io $ readMVar $ configMVar mvs

-- | Fork for the Memory monad.
forkMe :: (MonadReader r m, MonadIO m) => ReaderT r IO () -> m ThreadId
forkMe m = do
    r <- ask
    liftIO . forkIO $ runReaderT m r

-- | Allow function then do.
allowThen :: (Funcs -> Bool) -- ^ allowFunc function, for example `allowTitle'.
          -> Memory (Message String) -- ^ A message inside the Memory monad.
          -> Memory (Message String)
allowThen f m = do
    config <- asks getConfig
    meta <- asks getMeta
    let servers = serversC config
        mServer :: Maybe Server
        mServer = findServer servers $ getServer meta
        mFuncs = fmap (insLookup (getDestino meta) . allowedFuncs) mServer
        func :: Bool
        func = safeFromMaybe False . fmap f $ M.join mFuncs
    if func
        then m
        else return EmptyMsg

-- | Inject a server URL into a Meta
injectServ :: String -> Meta -> Meta
injectServ str (Meta d n u h c _ o) = Meta d n u h c str o

-- | Inject a list of Events into Config
injectEvents events c = c { eventsC = events }

-- this is used?
injectTemp temp (Event f r ti c s _) = Event f r ti c s temp

-- | Inject a Meta into MetaConfig
injectMeta :: Meta -> MetaConfig -> MetaConfig
injectMeta meta (MetaConfig _ config) = MetaConfig meta config

-- | Apply a function to the Config within MetaConfig
modConfig :: (Config -> Config) -> MetaConfig -> MetaConfig
modConfig f (MetaConfig meta config) = MetaConfig meta $ f config

-- | Apply a function to the userlist within a meta
mapMetaUserlist :: ([String] -> [String]) -> Meta -> Meta
mapMetaUserlist f (Meta de ni us ho ch se li) = Meta de ni us ho ch se $ f li

-- | Apply a function to the servers within a config
mapConfigServers :: ([Server] -> [Server]) -> Config -> Config
mapConfigServers f c = c { serversC = f (serversC c) }

-- | Get a server's meta from the config. Return emptyMeta on fail.
getConfigMeta :: Config
              -> String -- server
              -> String -- channel
              -> Meta -- server's meta
getConfigMeta config s c =
    let se = serversC config
        mm = M.join $ listToMaybe $ do
        server <- se
        guard $ serverURL server == s
        return $ listToMaybe $ do
            m <- serverMetas server
            guard $ getDestino m == c
            return m
    in fromJust $ mm <|> Just emptyMeta

getServerNick :: Memory String
getServerNick = do
    meta <- asks getMeta
    servers <- asks (serversC . getConfig)
    let serverurl = getServer meta
        mnick = listToMaybe $ do
            server <- servers
            guard $ serverURL server `insEq` serverurl
            return $ serverNick server
    return . fromJust $ mnick <?> Just ""

-------------------------------------------------------------------------------
-- ** Text.XML.Light utils
-------------------------------------------------------------------------------

-- | Find elements by attributes
findElementsAttrs :: QName -> [Attr] -> Element -> [Element]
findElementsAttrs name attrs element = filterElements match element
  where match :: Element -> Bool
        match (Element name' attrs' _ _) =
            if qName name == qName name' || null (qName name) then
                if attrs `compare_` attrs'
                    then True
                    else False
            else False
        compare_ x y =
            let f x' acc | x' `elem` y = (True, x') : acc
                         | otherwise = (False, x') : acc
            in and $ map fst $ foldr f [] x

-- | Find first element by attributes
findElementAttrs :: QName -> [Attr] -> Element -> Maybe Element
findElementAttrs name attrs element =
    listToMaybe $ findElementsAttrs name attrs element

-- | Find elements inside an element
findElementsIn :: QName -> Element -> [Element]
findElementsIn q e =
    let elems = (contentsToEls . elContent) e
    in M.join $ map (findElements q) elems

-- TODO:
---- Convert Elem to Text and concat it into one single Elem with loads of Text
---- then run strContent' over it.
------ Is this still relevant? ヽ(゜∀゜)ノ
-- | Recursively gets text of element and all children and handles <br>
-- elements as spaces instead.
elemsText :: Element -> String
elemsText e =
    let name = elName e
        attrs = elAttribs e
        content = elemsText' . elContent $ e
        line = elLine e
        elem' = Element name attrs [content] line
    in strContents elem'
  where elemsText' :: [Text.XML.Light.Content] -> Text.XML.Light.Content
        elemsText' cs =
            let f x acc = case () of
                  _ | isElem x -> if isBR $ fromElem x
                        then " " : (fold . elContent . fromElem) x ++ acc
                        else (fold . elContent . fromElem) x ++ acc
                    | isText x -> textToString x : acc
                    | otherwise -> acc
                fold x = foldr f [] x
            in genText . concat . fold $ cs
        isText (Text _) = True
        isText _ = False
        isElem (Elem _) = True
        isElem _ = False
        isBR (Element (QName n _ _) _ _ _) =
            n `elem` ["br", "bR", "Br", "BR"]
        isBR _ = False
        fromElem (Elem x) = x
        textToString (Text x) = cdData x
        genText x = Text $ CData { cdVerbatim = CDataText
                                 , cdData = x
                                 , cdLine = Nothing
                                 }

-- | Get the text from contents of an element
strContents (Element _ _ content _) =
    unwords $ map (cdData . fromText) . filter isText $ content
  where isText (Text _) = True
        isText _ = False

-- | Get the text from contents
elemText :: [Text.XML.Light.Content] -> String
elemText content' = foldr (\x acc -> if isElemText x
                                        then acc ++ (cdData . fromText $ x)
                                        else acc) "" content'

-- | Safe from maybe to element
fromMaybeElement :: Maybe Element -> Element
fromMaybeElement (Just a)   = a
fromMaybeElement Nothing    = Element (QName "" Nothing Nothing) [] [] Nothing

-- | Unsafe from Text to CData
fromText :: Text.XML.Light.Content -> CData
fromText (Text a) = a
fromText _        = error "Not `Text'"

-- | Get `Element's from contents
contentsToEls :: [Text.XML.Light.Content] -> [Element]
contentsToEls xs =
    let f (Elem x) acc = x : acc
        f _ acc = acc
    in foldr f [] xs

-- | Is the content `Text'?
isElemText :: Text.XML.Light.Content -> Bool
isElemText (Text _) = True
isElemText _        = False

-------------------------------------------------------------------------------
-- ** Maybe utils
-------------------------------------------------------------------------------

-- | Safe from maybe to string
fromMaybeString :: Maybe String -> String
fromMaybeString (Just a) = a
fromMaybeString Nothing  = []

-- | Generic safe from @Maybe a@
safeFromMaybe :: a -> Maybe a -> a
safeFromMaybe _ (Just a) = a
safeFromMaybe a Nothing = a

-- | Read that returns Nothing on error
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-------------------------------------------------------------------------------
-- ** Config.hs helper functions
-------------------------------------------------------------------------------

-- | A function emulating KawaiiBot's IRC operator \`++' for Config.hs.
add :: Memory (Message String) -> Memory (Message String) -> Memory String
add x y = do
    x' <- fmap fromMsg x
    y' <- fmap fromMsg y
    return $ x' ++ " " ++ y'

-- | A function emulating KawaiiBot's IRC operator \`>>' for Config.hs.
bind :: Memory (Message String) -> Memory (Message String) -> Memory String
bind x y = do
    void x
    fmap fromMsg y

-- | A function emulating KawaiiBot's IRC operator `->' for Config.hs.
pipe :: Memory (Message String)
     -> ([String] -> String -> Memory (Message String))
     -> [String]
     -> String
     -> Memory String
pipe f g x y = do
    f' <- fmap fromMsg f
    fmap fromMsg . g x $ unwords [y, f']

-- | Takes the String out of Message
plain :: Memory (Message String)-> Memory String
plain x = fmap fromMsg x

-------------------------------------------------------------------------------
-- ** Applicative utils
-------------------------------------------------------------------------------

-- | if x == empty then y else x
(<?>) :: (Applicative m, Alternative m, Eq (m a)) => m a -> m a -> m a
x <?> y | x /= empty = x
        | otherwise = y

(>|<) :: (Applicative m, Alternative m, Eq (m a)) => m a -> m a -> m a
x >|< y | x == empty = x
        | y == empty = y
        | otherwise = x <|> y

-------------------------------------------------------------------------------
-- ** Parser utils
-------------------------------------------------------------------------------


parseConf :: [String] -> [(String, [String])]
parseConf [] = []
parseConf ([]:xs) = parseConf xs
parseConf (x:xs) =
    let title = takeWhile (/= ':') x
        (start, rest) = inparse xs
    in (title, start) : parseConf rest
  where inparse :: [String] -> ([String], [String])
        inparse [] = ([], [])
        inparse ([]:xs) = inparse xs
        inparse ((' ':ys):xs) =
            let x = dropWhile (== ' ') ys
            in if null x
                then inparse xs
                else mapFst ([x] ++) $ inparse xs
        inparse xs = ([], xs)

-------------------------------------------------------------------------------
-- ** IO utils
-------------------------------------------------------------------------------

safeWriteFile :: FilePath -> String -> IO ()
safeWriteFile f s = do
    (t, h) <- openTempFile "/tmp" ""
    hPutStr h s
    hClose h
    copyFile t f
    removeFile t

-- | Read a file strictly
sReadFile :: FilePath -> IO String
sReadFile s = do
    h <- openFile s ReadMode
    c <- hGetContents h
    c `seq` hClose h
    return c

-------------------------------------------------------------------------------
-- ** Eq utils
-------------------------------------------------------------------------------

-- | Case insensitive (`==') function limited to `String's.
insEq :: String -> String -> Bool
insEq [] [] = True
insEq [] _ = False
insEq _ [] = False
insEq (x:xs) (y:ys) = x `elem` [toUpper y, toLower y] && insEq xs ys

-- | Case insensitive `elem' function limited to `String's.
insElem :: String -> [String] -> Bool
insElem _ [] = False
insElem x (y:ys) = x `insEq` y || x `insElem` ys

-- | Case insensitive `isInfixOf' function limited to `String's.
insIsInfixOf :: String -> String -> Bool
insIsInfixOf xs ys = f xs xs ys
  where f _ [] _ = True
        f _ _ [] = False
        f o (x:xs) (y:ys) | [x] `insEq` [y] = f o xs ys
                          | otherwise = f o o ys

-- | Are all `Char's digits?
isDigits :: String -> Bool
isDigits = and . map isDigit

takeWhileOrd :: (a -> a -> Bool) -> [a] -> [a]
takeWhileOrd f xs = g xs []
  where g [] _ = []
        g (x:xs) [] = x : g xs [x]
        g (x:xs) [y] | f x y = x : g xs [x]
                     | otherwise = []

maybeInsEq :: String -> Maybe String -> Bool
maybeInsEq x Nothing = False
maybeInsEq x (Just y) = x `insEq` y

-------------------------------------------------------------------------------
-- ** Yuri utils
-------------------------------------------------------------------------------

-- |
totalStats :: Yuri -> Int
totalStats (Yuri _ _ Nothing (x, y, z)) = x + y + z
totalStats (Yuri _ _ (Just (_, (x',y',z'))) (x,y,z)) = x + x' + y + y' + z + z'

-- |
oneStat :: ((Int, Int, Int) -> Int) -> Yuri -> Int
oneStat f (Yuri _ _ Nothing s) = f s
oneStat f (Yuri _ _ (Just (_, s')) s) = f s + f s'

-- |
applyStats :: (Int, Int, Int) -> Yuri -> Yuri
applyStats (s'1, s'2, s'3) (Yuri n i w (s1,s2,s3)) =
    Yuri n i w (s1 `plus` s'1, s2 `plus` s'2, s3 `plus` s'3)
  where plus x y | x + y `elem` [0 .. 30] = x + y
                 | otherwise = x

-- |
applyWep :: (Int, Int, Int) -> Maybe (String, (Int, Int, Int)) -> (Int, Int, Int)
applyWep stats Nothing = stats
applyWep (x,y,z) (Just (wep, (x',y',z'))) = (x + x', y + y', z + z')

-- |
getWepStats :: Maybe (String, (Int, Int, Int)) -> (Int, Int, Int)
getWepStats Nothing = (0, 0, 0)
getWepStats (Just (_, stats)) = stats

-- |
getWeaponStat :: Maybe (String, (Int, Int, Int)) -> Int
getWeaponStat Nothing = 0
getWeaponStat (Just (_, (x, y, z))) | x >= 1 = x
                                    | y >= 1 = y
                                    | z >= 1 = z

-- |
getBonusStat :: Maybe (String, (Int, Int, Int)) -> [Int]
getBonusStat Nothing = [0, 0, 0]
getBonusStat (Just (wep, (x, y, z)))
    | x >= 1 = [1, 0, 0]
    | y >= 1 = [0, 1, 0]
    | z >= 1 = [0, 0, 1]
    | otherwise = [0, 0, 0]

-- |
showApplyWep :: (Int, Int, Int) -> Maybe (String, (Int, Int, Int)) -> String
showApplyWep stats Nothing = show stats
showApplyWep (x, y, z) (Just (_, (x', y', z')))
    | x' >= 1 = concat [ "(", show x, "\ETX09+", show x', "\ETX, ", show y, ", ", show z, ")" ]
    | y' >= 1 = concat [ "(", show x, ", ", show y, "\ETX09+", show y', "\ETX, ", show z, ")" ]
    | z' >= 1 = concat [ "(", show x, ", ", show y, ", ", show z, "\ETX09+", show z', "\ETX)" ]
    | otherwise = "\ETX05ERROR\ETX"

-- |
isYuris :: String -> String -> Yuris -> Bool
isYuris s c (Yuris s' c' _) = s == s' && c == c'

-- |
ownerModTime :: YuriOwner -> Double -> YuriOwner
ownerModTime (YuriOwner n h _ ys) t = YuriOwner n h t ys

-- |
isOwner :: String -> YuriOwner -> Bool
isOwner n (YuriOwner n' _ _ _) = n `insEq` n'

-- |
addOwners :: Yuris -> [YuriOwner] -> Yuris
addOwners (Yuris s c _) os = Yuris s c os

-- |
addYuri :: YuriOwner -> Yuri -> YuriOwner
addYuri (YuriOwner n h t ys) y = YuriOwner n h t $ y : ys

-- |
writeYuriss :: [Yuris] -> Memory ()
writeYuriss ys = do
    yuripath <- asks $ yuriPathC . getConfig
    let yurispath = yuripath ++ " yuris"
    liftIO $ safeWriteFile yurispath $ unlines $ map show ys

-- |
statsMinus :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
statsMinus (x, y, z) (x', y', z') =
    let x'' = x - (x' `div` 2) - (floor $ 1.3 * fromIntegral y')
        y'' = y - (y' `div` 2) - (floor $ 1.3 * fromIntegral z')
        z'' = z - (z' `div` 2) - (floor $ 1.3 * fromIntegral x')
    in (x'', y'', z'')

-- |
showWep :: Yuri -> String
showWep (Yuri nick img Nothing stats) = show stats
showWep (Yuri nick img wep stats) =
    unwords [showApplyWep stats wep, wepName wep]

-- |
wepName :: Maybe (String, (Int, Int, Int)) -> String
wepName Nothing = ""
wepName (Just (wep, _)) = wep

-------------------------------------------------------------------------------
-- ** Unsorted utils
-------------------------------------------------------------------------------

-- | Generic safe `read' function
safeRead :: Read a => String -> a -> a
safeRead x y = fromJust $ maybeRead x <|> Just y
