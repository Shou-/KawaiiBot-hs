
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

{-# LANGUAGE OverloadedStrings #-}

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
import Data.Time.Clock.POSIX

import Network

import System.IO
-- }}}

-- TODO
-- - Several nicks -> Try until works -? None available -> Something
-- - Replace `put' with `pas' which takes a (ServersConfig -> Memory ())
--   function as an argument and attempts to apply it if it fails.
-- - Replace `see' and `put' with `get' and `fill' where possible.

-- XXX
-- - We can assume that the server exists if invoked from a function that
--   holds a server. Instead, pass the server. Stop using Map lookups
--   everywhere.

-- FIXME
-- - Delay before response after connecting to servers.

-- {{{ data
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

data User = User Text Text Text deriving (Show)

data ServerStatus = Connected | Connecting | Disconnected deriving (Show, Eq)

isDisconnected :: Server -> Bool
isDisconnected s = serverStatus s == Disconnected

isConnecting :: Server -> Bool
isConnecting s = serverStatus s == Connecting

isConnected :: Server -> Bool
isConnected s = serverStatus s == Connected

-- | IRC server data
data Server = Server { serverURL :: Text
                     , serverPort :: PortNumber
                     , serverNick :: Text
                     , serverChans :: Map Text [Text]
                     , serverNSPass :: Text
                     , serverHandle :: Maybe Handle
                     , serverTStamp :: Int
                     , serverRStamp :: Int
                     , serverStatus :: ServerStatus
                     } deriving (Show)

-- | Socket clients.
type Client = (Text, Handle)

-- | Last modified timestamp, list of `Client's.
type Clients = (Int, [Client])

data ServersConfig = ServersConfig { serversC :: Map Text Server
                                   , clientsC :: Clients
                                   , verbosityC :: Int
                                   } deriving (Show)

type ConfigMVar = TMVar ServersConfig

type Memory = ReaderT ConfigMVar IO
-- }}}

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
        in Server ur po ni ch ns Nothing 0 0 Disconnected

-- {{{ Debugging

errorLog :: FilePath
errorLog = KT.errorPathC KC.config ++ "core-errors.log"

putError :: [Text] -> Memory ()
putError ts = liftIO $ do
    T.appendFile errorLog $ T.unwords ts `T.snoc` '\n'
    T.putStrLn $ T.unwords ts

-- }}}

-- {{{ Parsers
parseUser :: Parser (Text, Text, Text)
parseUser = do
    nick <- A.takeWhile1 (/= '!')
    A.char '!'
    name <- A.takeWhile1 (/= '@')
    A.char '@'
    host <- A.takeWhile1 (/= ' ')
    return (nick, name, host)

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
                A.char ':'
                newnick <- A.takeWhile1 (/= ' ')
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
                A.string " = "
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
connect :: Text -> Memory ()
connect url = do
    servers <- serversC <$> see
    let mserver = M.lookup url servers
    case mserver of
        Just (Server u p n c r Nothing t rt s) -> do
            handle <- connectToIRC u p
            ircWrite url handle $ T.unwords ["NICK", n]
            ircWrite url handle $ T.concat ["USER ", n, " 0 * :", n]
            sc <- get
            let server = Server u p n c r (Just handle) t rt Connecting
            let servers' = M.insert url server $ serversC sc
            fill $ sc { serversC = servers' }
            void . forkMe $ ircListen u handle
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
        Just s -> when (isDisconnected s || isConnected s) $ do
            case serverHandle s of
                Just h -> do
                    e <- tryHClose h
                    either (putError . (: []) . T.pack . show) return e
                Nothing -> do
                    putError [ "reconnect:"
                             , server
                             , "doesn't have a handle."
                             ]
            let s' = s { serverHandle = Nothing, serverStatus = Disconnected }
            ServersConfig ss cc vv <- see
            let ss' = M.insert server s' ss
            put $ ServersConfig ss' cc vv
            connect server
        Nothing -> putError [ "reconnect:"
                            , server
                            , "does not exist in Config."
                            ]
  where
    tryHClose :: Handle -> Memory (Either SomeException ())
    tryHClose = liftIO . try . hClose

-- | Monitor IRC server timeouts by checking the last received and sent message
-- timestamps.
monitorTimeouts :: Memory ()
monitorTimeouts = forever $ do
    ss <- serversC <$> see
    time <- liftIO $ floor <$> getPOSIXTime
    forM_ (M.toList ss) $ \(s, server) -> unless (isConnecting server) $ do
        let tstamp = time - serverTStamp server
            rstamp = time - serverRStamp server
        case () of
          _ | tstamp > 185 && rstamp < 180 -> case serverHandle server of
                Just h -> void . forkMe $ ircWrite s h "PING irc.adelais.net"
                Nothing -> do
                    putError [ "monitorTimeouts:"
                             , s
                             , "has no Handle."
                             ]
                    void . forkMe $ reconnect $ serverURL server
            | tstamp + rstamp > 370 -> do
                putError [ "monitorTimeouts:"
                         , s
                         , "timed out. R + T."
                         ]
                void . forkMe $ reconnect $ serverURL server
            | tstamp > 200 -> do
                putError [ "monitorTimeouts:"
                         , s
                         , "timed out. T."
                         ]
                void . forkMe $ reconnect $ serverURL server
            | otherwise -> return ()
    liftIO $ threadDelay $ 10^6

-- | Listen to an IRC `Handle' and then parse the message and respond
-- appropriately.
ircListen :: Text -> Handle -> Memory ()
ircListen server handle = do
    let tryGetLine :: Memory (Either SomeException TL.Text)
        tryGetLine = liftIO . try $ TL.hGetLine handle
    e <- tryGetLine
    case e of
        Right line -> do
            liftIO $ print line
            let ircmsg = A.parse parseIRCMsg line
            case resultToMessage ircmsg of
                Privmsg (User nick name host) dest msg -> return ()
                Join (User nick name host) dest -> do
                    modUserlist (nick :) server dest
                Part (User nick name host) dest msg -> do
                    modUserlist (filter (/= nick)) server dest
                Quit (User nick name host) msg -> do
                    mserver <- M.lookup server . serversC <$> see
                    case mserver of
                        Just s -> forM_ (M.keys $ serverChans s) $ \chan -> do
                            modUserlist (filter (/= nick)) server chan
                        Nothing -> do
                            putError [ "ircListen:"
                                     , server
                                     , "not in Servers."
                                     ]
                Nick (User nick name host) newnick -> do
                    mserver <- M.lookup server . serversC <$> see
                    case mserver of
                        Just s -> do
                            forM_ (M.keys $ serverChans s) $ \chan -> do
                                let f xs = if nick `elem` xs
                                            then newnick : filter (/= nick) xs
                                            else xs
                                modUserlist f server chan
                        Nothing -> do
                            putError [ "ircListen:"
                                     , server
                                     , "not in Servers."
                                     ]
                Userlist dest users -> do
                    sc <- get
                    let mserver = M.lookup server . serversC $ sc
                    case mserver of
                        Just s -> do
                            let f (k, v) = if T.toLower k == T.toLower dest
                                    then (dest, v)
                                    else (k, v)
                                us' = map f (M.toList $ serverChans s)
                                s' = s { serverChans = M.fromList us' }
                                ss' = M.insert server s' $ serversC sc
                            fill $ sc { serversC = ss' }
                        Nothing -> do
                            fill sc
                            putError [ "ircListen:"
                                     , server
                                     , "not in Servers."
                                     ]
                    modUserlist (\_ -> users) server dest
                Ping msg -> do
                    ircWrite server handle $ T.concat ["PONG :", msg]
                    updateRStamp server
                Welcome -> do
                    ss <- serversC <$> see
                    case M.lookup server ss of
                        Just s -> when (serverStatus s == Connecting) $ do
                            let s' = s { serverStatus = Connected }
                                ss' = M.insert server s' ss
                            sc <- see
                            put $ sc { serversC = ss' }
                        Nothing -> do
                            putError [ "ircListen: Welcome:"
                                     , server
                                     , "not in Config."
                                     ]
                    mc <- do
                        servers <- serversC <$> see
                        return $ serverChans <$> M.lookup server servers
                    let c = fromJust $ mc <|> Just M.empty
                    forM_ (M.keys c) $ \channel -> liftIO $ do
                        T.hPutStrLn handle $ T.unwords ["JOIN", channel]
                _ -> return ()
            let cleanline = T.dropWhile (== ':') . T.pack . TL.unpack $ line
            clientsWrite $ T.concat [ server, ":", cleanline ]
            updateTStamp server
            ircListen server handle
        Left e -> do
            -- Could not get line from handle
            putError [ "ircListen: "
                     , T.pack $ show e
                     ]
            reconnect server
  where
    resultToMessage (Done _ x) = x
    resultToMessage _ = VoidIRCMsg

updateTStamp :: Text -> Memory ()
updateTStamp s = do
    ServersConfig ss cc vv <- see
    let mserver = M.lookup s ss
    case mserver of
        Just server -> do
            time <- liftIO $ floor <$> getPOSIXTime
            let server' = server { serverTStamp = time }
                servers = M.insert s server' ss
            put $ ServersConfig servers cc vv
        Nothing -> do
            putError ["updateTStamp:", s, "not in ServersConfig."]

updateRStamp :: Text -> Memory ()
updateRStamp s = do
    ServersConfig ss cc vv <- see
    let mserver = M.lookup s ss
    case mserver of
        Just server -> do
            time <- liftIO $ floor <$> getPOSIXTime
            let server' = server { serverRStamp = time }
                servers = M.insert s server' ss
            put $ ServersConfig servers cc vv
        Nothing -> do
            putError ["updateRStamp:", s, "not in ServersConfig."]

modUserlist :: ([Text] -> [Text]) -> Text -> Text -> Memory ()
modUserlist f s c = do
    sc <- get
    let mserver = M.lookup s $ serversC sc
    case mserver of
        Just server -> do
            let mchan = M.lookup c $ serverChans server
            case mchan of
                Just chan -> do
                    let chan' = f chan
                        chans = M.insert c chan' $ serverChans server
                        server' = server { serverChans = chans }
                        servers = M.insert s server' $ serversC sc
                    liftIO $ print server'
                    fill $ sc { serversC = servers }
                Nothing -> do
                    fill sc
                    putError ["modUserlist:", c, "not in", s]
        Nothing -> do
            fill sc
            putError ["modUserlist:", s, "not in ServersConfig."]

ircWrite :: Text -> Handle -> Text -> Memory ()
ircWrite s h t = do
    liftIO $ putStr "<- " >> T.putStrLn t
    let tryPut :: Memory (Either SomeException ())
        tryPut = liftIO . try $ T.hPutStrLn h t
    e <- tryPut
    case e of
        Right _ -> return ()
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
    ServersConfig s (time, clients) v <- see
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
    time' <- liftIO $ floor <$> getPOSIXTime
    put $ ServersConfig s (time', clients') v
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
                                    updateRStamp serverurl
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
                                    putError [ chan
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
    time <- liftIO $ floor <$> getPOSIXTime
    ServersConfig ss (tsp, cs) v <- see
    put $ ServersConfig ss (time, (T.pack host, handle) : cs) v
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

initConnects :: Memory ()
initConnects = do
    sock <- liftIO . listenOn $ PortNumber 3737
    forkMe $ socketListen sock
    forkMe monitorTimeouts
    serverurls <- M.keys . serversC <$> see
    forM_ serverurls (forkMe . connect)

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
        servers = flip M.map staticServers $ \s -> s { serverTStamp = tsp
                                                     , serverRStamp = tsp
                                                     }
    configMVar <- atomically . newTMVar $ ServersConfig servers (tsp, []) verbosity
    forkIO $ runReaderT initConnects configMVar
    runReaderT keyboardInput configMVar
  where
    tryWipe :: IO ()
    tryWipe = do
        e <- try $ writeFile errorLog "" :: IO (Either SomeException ())
        either print return e

put :: ServersConfig -> Memory ()
put s = ask >>= void . liftIO . atomically . flip swapTMVar s

fill :: ServersConfig -> Memory ()
fill s = ask >>= void . liftIO . atomically . flip putTMVar s

see :: Memory ServersConfig
see = ask >>= liftIO . atomically . readTMVar

get :: Memory ServersConfig
get = ask >>= liftIO . atomically . takeTMVar
