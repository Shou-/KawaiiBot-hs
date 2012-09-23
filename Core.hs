
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
-- - forking at timeouts cause several connections if not fast enough. Make a
--   status for the Server. This will also stop timeout spam.
-- - Replace `put' with `pas' which takes a (ServersConfig -> Memory ()) function as an argument and attempts to apply it if it fails.

-- XXX
-- - We can assume that the server exists if invoked from a function that
--   holds a server. Instead, pass the server. Stop using Map lookups
--   everywhere.

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
               | VoidClientMsg
               deriving (Show)

data User = User Text Text Text deriving (Show)

data ServerStatus = Connected | Connecting | Disconnected deriving (Show, Eq)

isDisconnected :: Server -> Bool
isDisconnected s = serverStatus s == Disconnected

isConnecting :: Server -> Bool
isConnecting s = serverStatus s == Connecting

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
            "003" -> do
                return Welcome
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
        _ -> return VoidClientMsg
-- }}}

-- {{{ IRC
connect :: Text -> Memory ()
connect url = do
    ServersConfig servers cs v <- see
    let mserver = M.lookup url servers
    mserver' <- case mserver of
        Just (Server u p n c r Nothing t rt s) -> liftIO $ do
            handle <- connectToIRC u p
            T.hPutStrLn handle $ T.unwords ["NICK", n]
            T.hPutStrLn handle $ T.concat ["USER ", n, " 0 * :", n]
            let server = Server u p n c r (Just handle) t rt Connecting
            return $ Just server
        Nothing -> return Nothing
    case mserver' of
        Just server@(Server u p n c r (Just h) t rt s) -> do
            let servers' = M.insert url server servers
            put $ ServersConfig servers' cs v
            void . forkMe $ ircListen u h
        _ -> do
            -- Server does not exist in Config
            liftIO . T.putStrLn $ T.unwords [ "connect:"
                                            , url
                                            , "does not exist in Config."
                                            ]
            return ()
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
        Just s -> when (isDisconnected s) $ do
            case serverHandle s of
                Just h -> void $ tryHClose h
                Nothing -> return ()
            let s' = s { serverHandle = Nothing }
            ServersConfig ss cc vv <- see
            put $ ServersConfig (M.insert server s' ss) cc vv
            connect server
        Nothing -> liftIO . T.putStrLn $ T.unwords [ "reconnect:"
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
    forM_ (M.toList ss) $ \(_, server) -> unless (isConnecting server) $ do
        let tstamp = time - serverTStamp server
            rstamp = time - serverRStamp server
        case () of
          _ | tstamp > 185 && rstamp < 180 -> case serverHandle server of
                Just h -> void . forkMe $ ircWrite h "PING irc.adelais.net"
                Nothing -> do
                    liftIO . T.putStrLn $ T.unwords [ "monitorTimeouts:"
                                                    , serverURL server
                                                    , "has no Handle."
                                                    ]
                    void . forkMe $ reconnect $ serverURL server
            | tstamp + rstamp > 370 -> do
                liftIO . T.putStrLn $ T.unwords [ "monitorTimeouts:"
                                                , serverURL server
                                                , "timed out. R + T."
                                                ]
                void . forkMe $ reconnect $ serverURL server
            | tstamp > 200 -> do
                liftIO . T.putStrLn $ T.unwords [ "monitorTimeouts:"
                                                , serverURL server
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
                Privmsg (User nick name host) dest msg -> do
                    clientsWrite $ T.concat [server, T.pack . TL.unpack $ line]
                Join (User nick name host) dest -> do
                    modUserlist (nick :) server dest
                Part (User nick name host) dest msg -> do
                    modUserlist (filter (/= nick)) server dest
                Quit (User nick name host) msg -> do
                    mserver <- M.lookup server . serversC <$> see
                    case mserver of
                        Just s -> forM_ (M.keys $ serverChans s) $ \chan -> do
                            modUserlist (filter (/= nick)) server chan
                        Nothing -> liftIO $ do
                            T.putStrLn $ T.unwords [ "ircListen:"
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
                        Nothing -> return ()
                Userlist dest users -> do
                    clientsWrite $ T.concat [ "getuserlist "
                                            , server, " "
                                            , dest, ":"
                                            , T.unwords users
                                            ]
                    modUserlist (\_ -> users) server dest
                Ping msg -> do
                    ircWrite handle $ T.concat ["PONG :", msg]
                    updateRStamp server
                Welcome -> do
                    ss <- serversC <$> see
                    case M.lookup server ss of
                        Just s -> do
                            let s' = s { serverStatus = Connected }
                                ss' = M.insert server s' ss
                            sc <- see
                            put $ sc { serversC = ss' }
                        Nothing -> liftIO $ do
                            T.putStrLn $ T.unwords [ "ircListen: Welcome:"
                                                   , server
                                                   , "not in Config."
                                                   ]
                    mc <- do
                        servers <- serversC <$> see
                        return $ serverChans <$> M.lookup server servers
                    let c = fromJust $ mc <|> Just M.empty
                    forM_ (M.keys c) $ \channel -> liftIO $ do
                        T.hPutStrLn handle $ T.unwords ["JOIN", channel]
                _ -> do
                    clientsWrite $ T.concat [server, T.pack . TL.unpack $ line]
            updateTStamp server
            ircListen server handle
        Left e -> do
            -- Could not get line from handle
            liftIO $ putStr "ircListen: " >> print e
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
        Nothing -> liftIO $ do
            T.putStrLn $ T.unwords ["updateTStamp:", s, "not in ServersConfig."]

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
        Nothing -> liftIO $ do
            T.putStrLn $ T.unwords ["updateRStamp:", s, "not in ServersConfig."]

modUserlist :: ([Text] -> [Text]) -> Text -> Text -> Memory ()
modUserlist f s c = do
    ServersConfig ss cc vv <- see
    let mserver = M.lookup s ss
    case mserver of
        Just server -> do
            let mchan = M.lookup c $ serverChans server
            case mchan of
                Just chan -> do
                    let chan' = f chan
                        chans = M.insert c chan' $ serverChans server
                        server' = server { serverChans = chans }
                        servers = M.insert s server' ss
                    liftIO $ print server'
                    put $ ServersConfig servers cc vv
                Nothing -> liftIO $ do
                    T.putStrLn $ T.unwords ["modUserlist:", c, "not in", s]
        Nothing -> liftIO $ do
            T.putStrLn $ T.unwords ["modUserlist:", s, "not in ServersConfig."]

ircWrite :: Handle -> Text -> Memory ()
ircWrite h t = liftIO $ do
    putStr "<- " >> T.putStrLn t
    T.hPutStrLn h t
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
    let clients' = filter ((/= handle) . snd) clients
    time' <- liftIO $ floor <$> getPOSIXTime
    put $ ServersConfig s (time', clients') v

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
                                    ircWrite handle msg
                                    updateRStamp serverurl
                                Nothing -> liftIO $ do 
                                    T.putStrLn $ T.unwords [ "clientListen:"
                                                           , "No handle found:"
                                                           , serverurl
                                                           ]
                        Nothing -> liftIO $ do
                            T.putStrLn $ T.unwords [ "clientListen: ServerWrite"
                                                   , serverurl
                                                   , "not in Servers" ]
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
                                Nothing -> liftIO $ do
                                    T.putStrLn $ T.unwords [ chan
                                                           , "not in channels."
                                                           ]
                        Nothing -> liftIO $ do
                            T.putStrLn $ T.unwords [ "clientListen: GetUserlist"
                                                   , serverurl
                                                   , "not in Servers" ]
                _ -> do
                    liftIO $ do
                        putStr "clientListen: VoidClientMsg: "
                        print line
            clientListen host handle
        Left e -> do
            liftIO $ putStr "clientListen: " >> print e
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
    forM_ serverurls connect

keyboardInput :: Memory ()
keyboardInput = do
    line <- liftIO TL.getLine
    let ircmsg = A.parse parseIRCMsg line
    case ircmsg of
        Done x y -> liftIO $ print y
        _ -> liftIO $ print ircmsg
    keyboardInput

main :: IO ()
main = withSocketsDo $ do
    tsp <- floor <$> getPOSIXTime
    let verbosity = KT.verbosityC KC.config
        servers = flip M.map staticServers $ \s -> s { serverTStamp = tsp
                                                     , serverRStamp = tsp
                                                     }
    configMVar <- atomically . newTMVar $ ServersConfig servers (tsp, []) verbosity
    forkIO $ runReaderT initConnects configMVar
    runReaderT keyboardInput configMVar

put :: ServersConfig -> Memory ()
put s = ask >>= void . liftIO . atomically . flip swapTMVar s

see :: Memory ServersConfig
see = ask >>= liftIO . atomically . readTMVar
