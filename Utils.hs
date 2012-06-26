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

{-
TODO:
-- Clean up this file, sort functions and such by category.
-}


{-# LANGUAGE DoAndIfThenElse,
    MultiParamTypeClasses,
    TypeSynonymInstances,
    FlexibleInstances #-}

module Utils where


import Codec.Binary.UTF8.String (encodeString, decodeString)
import Control.Exception
import Data.Maybe (fromJust, listToMaybe)
import Data.String.Utils (split, replace)
import qualified Data.Text as T
import Network.CGI (urlEncode, urlDecode)
import Network.Curl
import Text.XML.Light


class PyFormat s r where
    (%) :: s -> r -> s

instance PyFormat String [(String, String)] where
    (%) x y = foldr (\(s1, s2) x' -> replace ("%(" ++ s1 ++ ")") s2 x') x y

instance PyFormat String (String, String) where
    (%) x (s1, s2) = replace ("%(" ++ s1 ++ ")") s2 x

instance PyFormat String String where
    (%) x y = replace "%s" y x

instance PyFormat T.Text [(String, String)] where
    (%) x y = let text = T.unpack x
              in T.pack $ text % y

type Variables = [(String, [(String, [(String, String, Variable String)])])]

data Message = ChannelMsg String
             | UserMsg String
             | EmptyMsg
             deriving (Eq, Show)

data Variable a = Regular a
                | Personal a
                | Immutable a
                | Reminder a
                | EmptyVar
                deriving (Show, Read, Eq)

data Server = Server { serverPort :: Int
                     , serverURL :: String
                     , serverChans :: [String]
                     , serverNick :: String
                     } deriving (Show, Read)

data Meta = Meta {
    getDestino :: String,
    getUsernick :: String,
    getUsername :: String,
    getHostname :: String,
    getChannels :: [String],
    getServer :: String,
    getOwnNick :: String
    } deriving (Show, Read, Eq)

type HistMsg = (String, String, String, String)

botversion = "KawaiiBot 0.1.1"

-- Message utilities
fromMsg (ChannelMsg x) = x
fromMsg (UserMsg x) = x
fromMsg EmptyMsg = []

msgtype (UserMsg _) = UserMsg
msgType _ = ChannelMsg

isChannelMsg (ChannelMsg _) = True
isChannelMsg _ = False

-- Server utils
getByServerURL :: [Server] -> String -> Maybe Server
getByServerURL xs s = foldr f Nothing xs
  where f = (\x acc -> if serverURL x == s then Just x else acc)

cutoff :: String -> Int -> String
cutoff s n = if length s > n then (take n s) ++ "..." else s

joinUntil :: String -> [String] -> Int -> String
joinUntil str strs n =
    let str' = snd $ foldl (\(status, acc) x -> let acc' = acc ++ x ++ str
                                                in if status && length acc' <= n then (True, acc') else (False, acc)) (True, "") strs
    in take (length str' - length str) str'

isNum :: String -> Bool
isNum [] = False
isNum str = foldr (\x acc -> if x `elem` ['0' .. '9'] then acc else False) True str

tupleGet :: [(String, [a])] -> String -> (String, [a])
tupleGet tupleList str = foldr (\(x, y) acc -> if x == str then (x, y) else acc) ("", []) tupleList

tupleMaybeGet :: [(String, a)] -> String -> Maybe (String, a)
tupleMaybeGet list str = foldr (\(x, y) acc -> if x == str then Just (x, y) else acc) Nothing list

tupleListGet :: [(String, [a])] -> String -> [a]
tupleListGet tupleList str = foldr (\(x, y) acc -> if x == str then y else acc) [] tupleList

tupleInject :: (String, a) -> [(String, a)] -> [(String, a)]
tupleInject var@(name, _) vars =
    let (status, list) = foldr (\var'@(name', _) (status', acc) -> if name == name' then (True, var : acc) else (status', var' : acc)) (False, []) vars
    in if status then list else var : list

tupleVarInject :: [(String, String, Variable a)] -> (String, String, Variable a) -> [(String, String, Variable a)]
tupleVarInject vars var@(name, _, _) =
    let (status, list) = foldr (\var'@(name', _, _) (status', acc) -> if name == name' then (True, var : acc) else (status', var' : acc)) (False, []) vars
    in if status then list else var : list

splits :: [Char] -> String -> [String]
splits (x:xs) y = split [x] $ replaces xs [x] y
  where replaces (x':xs') y' s = replace [x'] y' (replaces xs' y' s)
        replaces [] _ s     = s
splits [] _ = []

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

unlines' :: [String] -> String
unlines' (x:[]) = x
unlines' xs     = unlines xs

bisectAt :: Eq a => a -> [a] -> ([a], [a])
bisectAt s x = let fst' = takeWhile (not . (== s)) x
                   snd' = dropWhile (not . (== s)) x
                   snd'' = if null snd' then snd' else tail snd'
                in (fst', snd'')

curlGetString' :: FilePath -> [CurlOption] -> IO String
curlGetString' url opt =
    let errorfunc :: SomeException -> IO String
        errorfunc e = print e >> return ""
        opt' = opt ++ [ CurlFollowLocation True,
                        CurlTimeout 5,
                        CurlMaxFileSize (1024 * 1024 * 5),
                        CurlUserAgent (unwords [ "Mozilla/5.0",
                                                "(X11; Linux x86_64;",
                                                "rv:10.0.2)",
                                                "Gecko/20100101",
                                                "Firefox/10.0.2"])]
        curlFetch = do
            (status, response) <- curlGetString url opt'
            print status
            return response
    in handle errorfunc curlFetch

findElementsAttrs :: Element -> QName -> [Attr] -> [Element]
findElementsAttrs element name attrs = filterElements match element
  where match :: Element -> Bool
        match (Element name' attrs' _ _) =
            if qName name == qName name' || null (qName name) then
                if attrs `compare_` attrs'
                    then True
                    else False
            else False
        compare_ :: Eq a => [a] -> [a] -> Bool
        compare_ x y = and $ map fst $ foldr (\x'' acc -> if x'' `elem` y then (True, x'') : acc else (False, x'') : acc) [] x

elemsText :: Element -> String
elemsText elem' =
    let elems' = onlyElems . elContent $ elem'
        strs = if null elems' then [] else ' ' : (unwords . map elemsText $ elems')
    in strContent elem' ++ strs

elemText :: [Text.XML.Light.Content] -> String
elemText content' = foldr (\x acc -> if isElemText x then acc ++ (cdData . fromText $ x) else acc) "" content'

fromMaybeElement :: Maybe Element -> Element
fromMaybeElement (Just a)   = a
fromMaybeElement Nothing    = Element (QName "" Nothing Nothing) [] [] Nothing

fromMaybeString :: Maybe String -> String
fromMaybeString (Just a) = a
fromMaybeString Nothing  = []

isElemText :: Text.XML.Light.Content -> Bool
isElemText (Text _) = True
isElemText _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

fromText :: Text.XML.Light.Content -> CData
fromText (Text a) = a
fromText _        = error "Not ``Text''"

urlEncode' :: String -> String
urlEncode' = urlEncode . encodeString

urlDecode' :: String -> String
urlDecode' = decodeString . urlDecode

safeRead :: Read a => String -> a -> a
safeRead x y = let maybeS = fmap fst . listToMaybe . reads $ x
               in if isNothing maybeS then y else fromJust maybeS

{-getDestinoMeta :: Server -> String -> Meta
getDestinoMeta server str = foldr (\meta acc -> if getDestino meta == str then meta else acc) emptyMessage $ getMetas server
  where emptyMessage = Meta [] [] [] [] [] (getChans server) (getServer server) (getNick server) []

-- Adds, or replaces if a Meta with the same Dest exists, Meta to a Server
injectMeta :: Server -> Meta -> Server
injectMeta (Server serverurl port nick nspw chans metas) meta =
    let dest = getDestino meta
        (status, metas') = foldr (\x (status', acc) -> if getDestino x == dest then (True, meta : acc) else (status', x : acc)) (False, []) metas
        metas'' = if status then metas' else meta : metas'
    in Server serverurl port nick nspw chans metas''-}

{--- Apply function `f' to all Metas' userlists in the Server
modUserlists :: ([(Char, String)] -> [(Char, String)]) -> Server -> Server
modUserlists f (Server serverurl port nick nspw chans metas) = let metas' = map (modUserlist f) metas in Server serverurl port nick nspw chans metas'-}

injectServ :: String -> Meta -> Meta
injectServ str (Meta d n u h c _ o) = Meta d n u h c str o

tupleVarGet :: [(String, String, Variable String)] -> String -> (String, String, Variable String)
tupleVarGet l s = foldr (\(x, y, z) acc -> if x == s then (x, y, z) else acc) ("", "", EmptyVar) l

readableVar :: Variable a -> Bool
readableVar (Regular _)     = True
readableVar (Immutable _)   = True
readableVar _               = False

writeableVar :: Variable a -> Bool
writeableVar (Regular _) = True
writeableVar (EmptyVar)  = True
writeableVar _           = False

fromVariable :: Variable a -> a
fromVariable (Regular a) = a
fromVariable (Personal a) = a
fromVariable (Immutable a) = a
fromVariable (Reminder a) = a
fromVariable (EmptyVar) = error "Cannot extract anything from empty variable."

