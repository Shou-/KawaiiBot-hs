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
{-# OPTIONS_HADDOCK prune #-}

module KawaiiBot.Utils where


import KawaiiBot.Types

import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Applicative
import Control.Concurrent
import Control.Exception
import qualified Control.Monad as M
import Control.Monad.Reader
import Data.Char (toUpper, toLower)
import Data.Maybe (fromJust, listToMaybe)
import Data.String.Utils (split, replace)
import qualified Data.Text as T
import Network.CGI (urlEncode, urlDecode)
import Network.Curl
import Text.XML.Light


botversion = "KawaiiBot 0.1.6"

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
findServer (server@(Server _ url _ _ _ _ _):xs) s = if s == url
    then Just server
    else findServer xs s

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

-- | A list of prefixes to remove from a string.
removePrefixes :: [String] -> String -> String
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

unlines' :: [String] -> String
unlines' (x:[]) = x
unlines' xs     = unlines xs

-- | Take until Int then add an ellipsis
cutoff :: String -> Int -> String
cutoff s n = if length s > n then (take n s) ++ "…" else s

-- | Join until length String >= Int
joinUntil :: String -> [String] -> Int -> String
joinUntil str strs n =
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
isGlobalVar (Global _ _ _) = True
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
-- ** Network.Curl utils
-------------------------------------------------------------------------------

-- | `curlGetString' with default list of `CurlOption'.
curlGetString' :: FilePath -> [CurlOption] -> IO String
curlGetString' url opt =
    let errorfunc :: SomeException -> IO String
        errorfunc e = print e >> return ""
        opt' = opt ++ [ CurlFollowLocation True
                      , CurlTimeout 5
                      , CurlMaxFileSize (1024 * 1024 * 5)
                      , CurlUserAgent $ unwords [ "Mozilla/5.0"
                                                , "(X11; Linux x86_64;"
                                                , "rv:10.0.2)"
                                                , "Gecko/20100101"
                                                , "Firefox/10.0.2"
                                                ]
                      ]
        curlFetch = do
            (status, response) <- curlGetString url opt'
            print status
            return response
    in handle errorfunc curlFetch

-- | Encode a URL
urlEncode' :: String -> String
urlEncode' = urlEncode . encodeString

-- | Decode an encoded URL
urlDecode' :: String -> String
urlDecode' = decodeString . urlDecode

-------------------------------------------------------------------------------
-- ** Memory utils
-------------------------------------------------------------------------------

-- | Fork for the Memory monad.
forkMe :: (MonadReader r m, MonadIO m) => ReaderT r IO () -> m ThreadId
forkMe m = do
    r <- ask
    liftIO . forkIO $ runReaderT m r

-- | Allow function then do
allowThen :: (Funcs -> Bool) -> Memory (Message String) -> Memory (Message String)
allowThen f m = do
    func <- getFunc f
    if func
        then m
        else return EmptyMsg

-- | 
getFunc :: (Funcs -> Bool) -> Memory Bool
getFunc f = do
    config <- asks getConfig
    meta <- asks getMeta
    let servers = serversC config
        mServer :: Maybe Server
        mServer = findServer servers (getServer meta)
                -- this needs to be case-insensitive
        mFuncs = fmap (insLookup (getDestino meta) . allowedFuncs) mServer
        func :: Bool
        func = safeFromMaybe False . fmap f . M.join $ mFuncs
    return func

-- | Inject a server URL into a Meta
injectServ :: String -> Meta -> Meta
injectServ str (Meta d n u h c _ o) = Meta d n u h c str o

-- | Inject a list of Events into Config
injectEvents events (Config s1 _ f1 f2 f3 s2 b2 i) =
    Config s1 events f1 f2 f3 s2 b2 i

-- this is used?
injectTemp temp (Event f r ti c s _) = Event f r ti c s temp

-- | Inject a Meta into MetaConfig
injectMeta :: Meta -> MetaConfig -> MetaConfig
injectMeta meta (MetaConfig _ config) = MetaConfig meta config

-- | Apply a function to the Config within MetaConfig
modConfig :: (Config -> Config) -> MetaConfig -> MetaConfig
modConfig f (MetaConfig meta config) = MetaConfig meta $ f config

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

-- | Is Nothing?
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

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

-------------------------------------------------------------------------------
-- ** Unsorted utils
-------------------------------------------------------------------------------

-- Make this lazier
-- | Case insensitive (`==') function.
insEq :: String -> String -> Bool
insEq a b = let (bool, v) = foldl f (True, a) b
            in if length v > 0 then False else bool
  where f (_, []) x = (False, [])
        f (b, (y:ys)) x = if x `elem` [toUpper y, toLower y]
            then (b, ys)
            else (False, [])

-- | Are all Chars numbers?
isNum :: String -> Bool
isNum [] = False
isNum str = foldr (\x acc -> if x `elem` ['0' .. '9'] then acc else False) True str

-- | Generic safe `read' function
safeRead :: Read a => String -> a -> a
safeRead x y = fromJust $ maybeRead x <|> Just y
