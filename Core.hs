
-- {{{ License
{-
A kawaii IRC bot that does useless things.
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
-- }}}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

-- {{{ imports
import qualified Config as KC

import qualified KawaiiBot.Types as KT
import KawaiiBot.Utils (forkMe)

import Control.Applicative
import Control.Exception as E
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader

import Data.Attoparsec.Text.Lazy (Parser, Result (..))
import qualified Data.Attoparsec.Text.Lazy as A
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Tuple

import Network

import System.IO
import System.IO.Error (isEOFError, IOError)
import System.Locale
import System.Timeout
-- }}}

-- TODO
-- - Several nicks -> Try until works -? None available -> append timestamp
-- - User MODEs being kept in the userlist.

-- XXX
-- - We can assume that the server exists if invoked from a function that
--   holds a server. Instead, pass the server. Stop using Map lookups
--   everywhere.

-- FIXME
-- - Delay before response after connecting to servers.
-- - Reconnecting doesn't work. Retries forever. Probably related to the Status.
--   In `connect', change the status to 'Connecting' before trying to connect
--   to the IRC server. Also, add a timestamp to 'Connecting' so that we can
--   retry if it takes too long. (15 sec)
-- - >hGetLine: invalid argument (invalid byte sequence)
--   what the hold hands
--   >you are better off reading IRC as binary, then micro-decode each small chunk and do your fine-grain error handling
--   >e.g. the iconv package gives quite a bit of control over how to handle invalid encodings

-- {{{ Data

class Replaceable a b where
    pas :: a -> b -> Memory ()

instance Replaceable (Server -> Server) Text where
    pas f server = do
        mvar <- ask
        e <- liftIO . atomically $ do
            sc <- takeTMVar mvar
            let ms = M.lookup server $ serversC sc
            flip (maybe . return $ Left ()) ms $ \s -> do
                let ss = M.insert server (f s) $ serversC sc
                putTMVar mvar $ sc { serversC = ss }
                return $ Right ()
        flip (flip either return) e . const $ do
            putError [ "pas:"
                     , server
                     , "not found in Servers."
                     ]

instance Replaceable ([Client] -> [Client]) Int where
    pas f t = do
        mvar <- ask
        liftIO . atomically $ do
            sc <- takeTMVar mvar
            let cs = let (_, cs) = clientsC sc in (t, f cs)
            putTMVar mvar $ sc { clientsC = cs }

data IRCMsg = Privmsg User Text Text
            -- ^ Privmsg User Destination Message
            | Join User Text
            -- ^ Join User Destination
            | Part User Text Text
            -- ^ Part User Destination Message
            | Kick User Text Text Text
            -- ^ Kick User Destination Nick Message
            | Nick User Text
            -- ^ Nick User Nick
            | Quit User Text
            -- ^ Quit User Message
            | Invite User Text Text
            -- ^ Invite User 
            | Userlist Text [Text]
            -- ^ Userlist Destination [Nick]
            | Ping Text
            -- ^ Ping Message
            | Pong Text
            -- ^ Pong Message
            | Welcome
            -- ^ Welcome message. Join channels here.
            | VoidIRCMsg
            deriving (Show)

data ClientMsg = ServerWrite Text Text
               -- ^ ServerWrite Server Message
               | GetUserlist Text Text
               -- ^ GetUserlist Server Channel
               | GetStatus
               -- ^ Sends the status of Kawaiibot, such as servers and channels
               -- that it is connected to.
               | VoidClientMsg
               deriving (Show)

-- | User Nick Name Host. Used in other data such as IRCMsg.
data User = User Text Text Text deriving (Show)

-- | Status of the server. Used in `Server' data.
data ServerStatus = Connected
                  | Connecting Int
                  | Disconnected
                  deriving (Show, Eq)

isDisconnected :: Server -> Bool
isDisconnected s = serverStatus s == Disconnected

isConnecting :: Server -> Bool
isConnecting s = case serverStatus s of
    Connecting _ -> True
    _ -> False

connectingTime :: Server -> Int -> Bool
connectingTime s t = case serverStatus s of
    Connecting t' -> t > t'
    _ -> False

isConnected :: Server -> Bool
isConnected s = serverStatus s == Connected

-- | IRC server data
data Server = Server { serverURL :: Text
                     , serverPort :: PortNumber
                     , serverNick :: Text
                     , serverChans :: Map Text [Text]
                     , serverNSPass :: Text
                     , serverHandle :: Maybe Handle
                     , serverStatus :: ServerStatus
                     } deriving (Show)

-- | Socket clients.
type Client = (Text, Handle)

-- | Last modified timestamp, list of `Client's.
type Clients = (Int, [Client])

-- | Servers config. Contains IRC servers, clients and the verbosity level.
-- It is used as KawaiiBot's "state", sitting inside a TMVar, which again is
-- inside a Reader monad.
data ServersConfig = ServersConfig { serversC :: Map Text Server
                                   , clientsC :: Clients
                                   , verbosityC :: Int
                                   } deriving (Show)

-- | TMVar that KawaiiBot reads to access the `ServersConfig' data.
type ConfigMVar = TMVar ServersConfig

-- | Memory reader monad, allowing KawaiiBot to access the `ConfigMVar' without
-- passing it to each function.
type Memory = ReaderT ConfigMVar IO
-- }}}

-- | Takes data from `KawaiiBot.config' and converts it into data used here
-- in the Core.
staticServers :: Map Text Server
staticServers = foldr serverMapper M.empty $ KT.serversC KC.config
  where
    serverMapper x xs = M.insert (T.pack $ KT.serverURL x) (serverToServer x) xs
    serverToServer (KT.Server port url chans nick nsp _ _ _) =
        let ur = T.pack url
            po = fromIntegral port
            ch = M.fromList $ zip (map T.pack chans) (cycle [[]])
            ni = T.pack nick
            ns = T.pack nsp
        in Server ur po ni ch ns Nothing Disconnected

-- {{{ Debugging

-- | File where error messages go.
errorLog :: FilePath
errorLog = KT.errorPathC KC.config ++ "core-errors.log"

-- | Put an error message into the `errorLog' file.
putError :: [Text] -> Memory ()
putError ts = liftIO $ do
    ft <- ftext
    T.appendFile errorLog $ ft `T.snoc` '\n'
    T.putStrLn ft
  where
    t = T.unwords ts
    timestamp = do
        time <- getCurrentTime
        return . T.pack $ formatTime defaultTimeLocale "%H:%M:%S%t" time
    ftext = flip T.append t <$> timestamp

putVerbose :: [Text] -> Memory ()
putVerbose ts = liftIO $ do
    T.putStrLn $ T.unwords ts

-- }}}

-- {{{ Parsers

-- | A parser that converts an IRC user in the `nick!name@host` format, to
-- `User' data.
parseUser :: Parser (Text, Text, Text)
parseUser = do
    nick <- A.takeWhile1 (/= '!')
    A.char '!'
    name <- A.takeWhile1 (/= '@')
    A.char '@'
    host <- A.takeWhile1 (/= ' ')
    return (nick, name, host)

-- | A parser that parses an IRC message and converts it to `IRCMsg' data.
parseIRCMsg :: Parser IRCMsg
parseIRCMsg = regular <|> status
  where
    regular = do
        A.char ':'
        sender <- A.takeWhile1 (/= ' ')
        A.char ' '
        cmd <- A.takeWhile (/= ' ')
        A.char ' '
        case cmd of
            "PRIVMSG" -> do
                dest <- A.takeWhile1 (/= ' ')
                A.string " :"
                msg <- A.takeText
                return $ Privmsg (user sender) dest msg
            "JOIN" -> do
                A.char ':'
                msg <- A.takeText
                return $ Join (user sender) msg
            "PART" -> do
                dest <- A.takeWhile1 (/= ' ')
                A.string " :"
                msg <- A.takeText
                return $ Part (user sender) dest msg
            "KICK" -> do
                dest <- A.takeWhile1 (/= ' ')
                A.char ' '
                kicknick <- A.takeWhile1 (/= ' ')
                A.string " :"
                msg <- A.takeText
                return $ Kick (user sender) dest kicknick msg
            "NICK" -> do
                newnick <- A.takeText
                return $ Nick (user sender) newnick
            "INVITE" -> do
                dest <- A.takeWhile1 (/= ' ')
                A.string " :"
                msg <- A.takeText
                return $ Invite (user sender) dest msg
            "QUIT" -> do
                A.char ':'
                msg <- A.takeText
                return $ Quit (user sender) msg
            "PONG" -> do
                A.skip (/= ':')
                A.char ':'
                msg <- A.takeText
                return $ Pong msg
            "001" -> return Welcome
            "002" -> return Welcome
            "003" -> return Welcome
            "004" -> return Welcome
            "005" -> return Welcome
            "353" -> do
                A.takeWhile1 (/= ' ')
                A.string " = " <|> A.string " @ "
                dest <- A.takeWhile1 (/= ' ')
                A.string " :"
                nicks <- A.takeText
                let nicks' = map (T.dropWhile (`elem` "+%@&~")) $ T.words nicks
                return $ Userlist dest nicks'
            _ -> return VoidIRCMsg
    status = do
        cmd <- A.takeWhile1 (/= ' ')
        A.char ' '
        case cmd of
            "PING" -> do
                A.char ':'
                server <- A.takeText
                return $ Ping server
            "ERROR" -> do
                A.char ':'
                action <- A.takeWhile1 (/= ':')
                A.string ": "
                msg <- A.takeText
                return VoidIRCMsg
            _ -> return VoidIRCMsg
    user :: Text -> User
    user t = case A.parse parseUser (TL.pack . T.unpack $ t) of
        Done _ (nick, name, host) -> User nick name host
        _ -> error "parseMsg: user: not a username"

-- | A parser that converts a client message to `ClientMsg' data.
parseClientMsg :: Parser ClientMsg
parseClientMsg = do
    cmd <- A.takeWhile1 (/= ' ')
    A.char ' '
    case cmd of
        "serverwrite" -> do
            server <- A.takeWhile1 (/= ':')
            A.char ':'
            msg <- A.takeText
            return $ ServerWrite server msg
        "getuserlist" -> do
            server <- A.takeWhile1 (/= ':')
            A.char ':'
            channel <- A.takeText
            return $ GetUserlist server channel
        "getstatus" -> do
            A.takeText
            return GetStatus
        _ -> return VoidClientMsg
-- }}}

-- {{{ IRC

-- | Connect to an IRC server.
connect :: Text -> Memory ()
connect url = do
    mserver <- M.lookup url . serversC <$> see
    case mserver of
        Just server -> do
            e <- liftIO . try . timeout (15 * 10 ^ 6) $ do
                connectToIRC (serverURL server) (serverPort server)
            case e of
                Right (Just h) -> do
                    let n = serverNick server
                    ircWrite url h $ T.unwords ["NICK", n]
                    ircWrite url h $ T.concat ["USER ", n, " 0 * :", n]
                    time <- liftIO $ (15 +) . floor <$> getPOSIXTime
                    flip pas url $ \s -> s { serverHandle = Just h
                                           , serverStatus = Connecting time
                                           }
                    ircListen (serverURL server) h False
                Right Nothing -> do
                    putError [ "connect:"
                             , url
                             , "timeout while connecting."
                             ]
                    reconnect url
                Left e -> do
                    putError [ "connect:"
                             , T.pack $ show (e :: SomeException)
                             , "(" `T.append` url `T.append` ")"
                             ]
                    reconnect url
        _ -> do
            putError [ "connect:"
                     , url
                     , "does not exist in Config."
                     ]
  where
    connectToIRC url p = liftIO $ do
        handle <- liftIO $ connectTo (T.unpack url) $ PortNumber p
        liftIO $ hSetEncoding handle utf8
        liftIO $ hSetBuffering handle LineBuffering
        liftIO $ hSetNewlineMode handle (NewlineMode CRLF CRLF)
        return handle

-- | Reconnect to an IRC server.
reconnect :: Text -> Memory ()
reconnect server = do
    mserver <- M.lookup server . serversC <$> see
    case mserver of
        Just s -> do
            putError [ "reconnect:"
                     , "Reconnecting to"
                     , server
                     , '(' `T.cons` T.pack (show $ serverStatus s) `T.snoc` ')'
                     ]
            time <- liftIO $ floor <$> getPOSIXTime
            let shouldReconnect =
                    isDisconnected s || isConnected s || connectingTime s time
            if shouldReconnect then do
                case serverHandle s of
                    Just h -> do
                        e <- tryHClose h
                        either (putError . (: []) . T.pack . show) return e
                    Nothing -> do
                        putError [ "reconnect:"
                                 , server
                                 , "doesn't have a handle."
                                 ]
                let f s = s { serverHandle = Nothing
                            , serverStatus = Disconnected
                            }
                pas f server
                connect server
            else reconnect server
        Nothing -> putError [ "reconnect:"
                            , server
                            , "does not exist in Config."
                            ]
  where
    tryHClose :: Handle -> Memory (Either SomeException ())
    tryHClose = liftIO . try . hClose

-- | Listen to an IRC `Handle' and then parse the message and respond
-- appropriately.
--
-- Its arguments take `Text' containing an IRC server address, an IRC server
-- handle and a boolean that decides whether or not it should check if the
-- server has been disconnected.
ircListen :: Text -> Handle -> Bool -> Memory ()
ircListen server handle rb = do
    let tout = if rb then 10 else 240
        tryGetLine :: Exception e => Memory (Either e (Maybe TL.Text))
        tryGetLine = liftIO . try . timeout (tout * 10 ^ 6) $ TL.hGetLine handle
    e <- tryGetLine
    case e of
        Right (Just line) -> do
            liftIO $ print line
            let ircmsg = A.parse parseIRCMsg line
            case resultToMessage ircmsg of
                Privmsg (User nick name host) dest msg -> return ()
                Join (User nick name host) dest -> do
                    flip pas server $ \s ->
                        modUserlist (nick :) s dest
                Part (User nick name host) dest msg -> do
                    flip pas server $ \s ->
                        modUserlist (filter (/= nick)) s dest
                Quit (User nick name host) msg -> do
                    flip pas server $ \s ->
                        let chans = M.keys $ serverChans s
                            f c s' =
                                modUserlist (filter (/= nick)) s' c
                        in foldr f s chans
                Nick (User nick name host) newnick -> do
                    flip pas server $ \s ->
                        let chans = M.keys $ serverChans s
                            f c s' =
                                let g xs = if nick `elem` xs
                                            then newnick : filter (/= nick) xs
                                            else xs
                                in modUserlist g s' c
                        in foldr f s chans
                Userlist dest users -> do
                    flip pas server $ \s ->
                        let f (k, v) = if T.toLower k == T.toLower dest
                                then (dest, v)
                                else (k, v)
                            us' = map f (M.toList $ serverChans s)
                            g s' = s' { serverChans = M.fromList us' }
                        in modUserlist (const users) s dest
                Ping msg -> do
                    ircWrite server handle $ T.concat ["PONG :", msg]
                Welcome -> do
                    flip pas server $ \s ->
                        if isConnecting s
                            then s { serverStatus = Connected }
                            else s
                    mc <- do
                        servers <- serversC <$> see
                        return $ serverChans <$> M.lookup server servers
                    flip (maybe $ return ()) mc $ \cs ->
                        forM_ (M.keys cs) $ \channel -> liftIO $ do
                            T.hPutStrLn handle $ T.unwords ["JOIN", channel]
                _ -> return ()
            let cleanline = T.dropWhile (== ':') . T.pack . TL.unpack $ line
            clientsWrite $ T.concat [ server, ":", cleanline ]
            ircListen server handle False
        Right Nothing -> do
            mserver <- M.lookup server . serversC <$> see
            flip (maybe $ return ()) mserver $ \s -> do
                if rb then do
                    putError [ "ircListen:"
                             , server
                             , "timed out. Reconnecting..."
                             ]
                    reconnect server
                else do
                    ircWrite server handle $ T.unwords [ "PING", server ]
                    ircListen server handle True
        Left e -> do
            putError [ "ircListen: "
                     , T.pack $ show (e :: SomeException)
                     ]
            reconnect server
  where
    resultToMessage (Done _ x) = x
    resultToMessage _ = VoidIRCMsg

-- | Modify the userlist of a channel in a server.
modUserlist :: ([Text] -> [Text]) -> Server -> Text -> Server
modUserlist f s c =
    let mchan = M.lookup c $ serverChans s
    -- putError ["modUserlist:", c, "not in", s]
    -- Use Debug.Trace if you need.
    in flip (maybe s) mchan $ \chan ->
        let chan' = f chan
            chans = M.insert c chan' $ serverChans s
        in s { serverChans = chans }

-- | Write to an IRC server handle.
ircWrite :: Text -> Handle -> Text -> Memory ()
ircWrite s h t = do
    let tryPut :: Memory (Either SomeException ())
        tryPut = liftIO . try $ T.hPutStrLn h t
    e <- tryPut
    case e of
        Right _ -> liftIO $ putStr "<- " >> T.putStrLn t
        Left e -> do
            putError [ "ircWrite:"
                     , T.pack $ show e ++ "."
                     , "Reconnecting to"
                     , s
                     ]
            reconnect s
-- }}}

-- {{{ Client
clientsWrite :: Text -> Memory ()
clientsWrite t = do
    clients <- clientsC <$> see
    liftIO $ putStr "-> " >> T.putStrLn t
    forM_ (snd clients) (void . tryPutLine . snd)
  where
    tryPutLine :: Handle -> Memory ()
    tryPutLine h = do
        let f :: Memory (Either SomeException ())
            f = liftIO $ try $ T.hPutStrLn h t
        e <- f
        case e of
            Right _ -> return ()
            Left e -> do
                removeClient h

removeClient :: Handle -> Memory ()
removeClient handle = do
    (time, clients) <- clientsC <$> see
    let f c@(ho, ha) acc = if ha == handle
                            then (Just ha, snd acc)
                            else fmap (c :) acc
        (mclient, clients') = foldr f (Nothing, []) clients
    case mclient of
        Just ha -> do
            e <- tryHClose ha
            case e of
                Right _ -> return ()
                Left e -> putError [ "removeClient", T.pack $ show e ]
        Nothing -> putError [ T.pack $ show handle
                            , "not found in Config"
                            ]
    (time' :: Int) <- liftIO $ floor <$> getPOSIXTime
    pas (const clients' :: ([Client] -> [Client])) time'
  where
    tryHClose :: Handle -> Memory (Either SomeException ())
    tryHClose = liftIO . try . hClose

clientListen :: HostName -> Handle -> Memory ()
clientListen host handle = do
    e <- tryGetLine handle
    case e of
        Right line -> do
            let clientmsg = A.parse parseClientMsg line
            case resultToMessage clientmsg of
                ServerWrite serverurl msg -> do
                    mserver <- M.lookup serverurl . serversC <$> see
                    case mserver of
                        Just server -> do
                            case serverHandle server of
                                Just handle -> do
                                    ircWrite serverurl handle msg
                                Nothing -> do
                                    putError [ "clientListen:"
                                             , "No handle found:"
                                             , serverurl
                                             ]
                                    reconnect serverurl
                        Nothing -> do
                            putError [ "clientListen: ServerWrite"
                                     , serverurl
                                     , "not in Servers"
                                     ]
                GetUserlist serverurl chan -> do
                    mserver <- M.lookup serverurl . serversC <$> see
                    case mserver of
                        Just server -> do
                            case M.lookup chan $ serverChans server of
                                Just users -> do
                                    let users' = T.unwords users
                                    clientsWrite $ T.concat [ "getuserlist "
                                                            , serverurl, " "
                                                            , chan, ":"
                                                            , users'
                                                            ]
                                Nothing -> do
                                    putError [ "clientListen:"
                                             , chan
                                             , "not in channels."
                                             ]
                        Nothing -> do
                            putError [ "clientListen: GetUserlist"
                                     , serverurl
                                     , "not in Servers"
                                     ]
                GetStatus -> do
                    time <- liftIO $ floor <$> getPOSIXTime
                    sc <- see
                    putError [ T.pack $ show time
                             , ":"
                             , T.pack $ show sc
                             ]
                _ -> do
                    putError [ "clientListen: VoidClientMsg: "
                             , T.pack . TL.unpack $ line
                             ]
            clientListen host handle
        Left e -> do
            putError [ "clientListen: ", T.pack $ show e ]
            removeClient handle
  where
    tryGetLine :: Handle -> Memory (Either SomeException TL.Text)
    tryGetLine = liftIO . try . TL.hGetLine
    resultToMessage (Done _ x) = x
    resultToMessage _ = VoidClientMsg

socketListen :: Socket -> Memory ()
socketListen sock = do
    (handle, host, _) <- liftIO $ accept sock
    liftIO $ do
        -- set handle character encoding to utf8
        hSetEncoding handle utf8
        -- line buffering instead of block buffering
        hSetBuffering handle LineBuffering
        -- newline mode to \r\n
        hSetNewlineMode handle (NewlineMode CRLF CRLF)
        putStrLn $ "Client " ++ host ++ " is listening."
    (time :: Int) <- liftIO $ floor <$> getPOSIXTime
    pas (((T.pack host, handle) :) :: ([Client] -> [Client])) time
    forkMe $ clientListen host handle
    servers <- serversC <$> see
    forM_ (M.toList servers) $ \(serverurl, server) -> do
        forM_ (M.toList $ serverChans server) $ \(chan, users) -> do
            let users' = T.unwords users
            liftIO $ T.hPutStrLn handle $ T.concat [ "getuserlist "
                                                   , serverurl, " "
                                                   , chan, ":"
                                                   , users'
                                                   ]
    socketListen sock
-- }}}

-- | Initialize the connections.
initConnects :: Memory ()
initConnects = do
    sock <- liftIO . listenOn $ PortNumber 3737
    forkMe $ socketListen sock
    serverurls <- M.keys . serversC <$> see
    forM_ serverurls (forkMe . connect)

-- | Get input from stdin.
keyboardInput :: Memory ()
keyboardInput = do
    line <- liftIO TL.getLine
    let cmsg = A.parse parseClientMsg line
    case cmsg of
        Done x y -> liftIO $ print y
        _ -> liftIO $ print cmsg
    keyboardInput

main :: IO ()
main = withSocketsDo $ do
    tryWipe
    tsp <- floor <$> getPOSIXTime
    let verbosity = KT.verbosityC KC.config
        servers = staticServers
    configMVar <- atomically . newTMVar $ ServersConfig servers (tsp, []) verbosity
    forkIO $ runReaderT initConnects configMVar
    runReaderT keyboardInput configMVar
  where
    tryWipe :: IO ()
    tryWipe = do
        e <- try $ writeFile errorLog "" :: IO (Either SomeException ())
        either print return e

-- | See reads the `TMVar', which only works when it is full.
see :: Memory ServersConfig
see = ask >>= liftIO . atomically . readTMVar
