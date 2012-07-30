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

module Main where


import Config

import KawaiiBot.Utils
import KawaiiBot.Types

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Data.List
import Data.Maybe
import Data.String.Utils (split)
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network
import System.Directory
import System.IO


-- TODO:
---- Show client disconnections instantly
---- If an IRC handle is closed, reconnect. Probably need another MVar.
------ Work has been started. Make sure handle actions are within `try' functions.
------ Doesn't reconnect properly.
-------- After the `irc.server.net has timed out' message is sent, handle is closed
-------- and KB reconnects but the IRC server does not respond. pls respond.
---- Additional internal functions available to the clients:
------ `connect irc.freenode.net' to connect to an IRC server. Disconnect too.
------ `getservers' to make it output which IRC servers are connected
------ `getchannels' is necessary for event channel black/whitelisting to work
---- Lower the CPU usage.
------ See: clientsListen
------ CPU usage used to be 0% with `threadDelay 10', see if we can lower it again.
---- JOINs are ignored if sent too early. Wait for the message with status code `001'
------ Status code `001' sometimes not sent? What about `002'?
-- Fix server timeouts. /FIXME
-- Finish the transition to CMemory

-- Simple function that writes to a handle and prints the bytestring
write :: Handle -> T.Text -> IO ()
write h bs = do
    T.hPutStrLn h bs
    T.putStrLn ('>' `T.cons` bs)

clientWrite :: CClient -> T.Text -> CMemory ()
clientWrite c@(clientHost, clientHandle) line = do
    clientsMVar' <- asks (clientsMVar . getcMVars)
    let put :: CMemory (Either SomeException ())
        put = io $ try (T.hPutStrLn clientHandle line)
    e <- put
    case e of
        Right _ -> return ()
        Left _ -> do
            let f c'@(clientHost', clientHandle') = if c == c'
                    then hClose clientHandle >> return False
                    else return True
            io . modifyMVar_ clientsMVar' $ filterM f

-- | Write to all open client handles
clientsWrite :: T.Text -> CMemory ()
clientsWrite line = do
    debug <- asks (verbosityC . getcConfig)
    mvar <- asks (clientsMVar . getcMVars)
    when (debug > 1) . io $ putStrLn "clientsWrite init..."
    hs <- io $ takeMVar mvar
    when (debug > 1) . io $ putStrLn "clientsWrite after takeMVar"
    mhs <- forM hs $ \(clientHost, clientHandle) -> do
        -- check if handle is still open
        e <- io $ try (T.hPutStrLn clientHandle line >> return clientHandle) :: CMemory (Either SomeException Handle)
        -- if it didn't return an exception, continue
        case e of
            -- return Just the host and handle
            Right clientHandle' -> return $ Just (clientHost, clientHandle')
                --`
            -- print a disconnection message and return Nothing
            Left _ -> do
                io (putStrLn (clientHost ++ " has disconnected."))
                return Nothing
    io . T.putStrLn $ '>' `T.cons` line
    let f x acc = if isJust x
            then fromJust x : acc
            else acc
    io . putMVar mvar $ foldr f [] mhs
  where isJust (Just _) = True
        isJust _ = False

-- | loop that handles client connections and adds them to the MVar list
socketlisten :: Socket -> CMemory ()
socketlisten sock = do
    clientsMVar' <- asks (clientsMVar . getcMVars)
    -- accept a socket
    (clientHandle, clientHost, _) <- io $ accept sock
    -- set handle character encoding to utf8
    io $ hSetEncoding clientHandle utf8
    -- line buffering instead of block buffering
    io $ hSetBuffering clientHandle LineBuffering
    -- newline mode to \r\n
    io $ hSetNewlineMode clientHandle (NewlineMode CRLF CRLF)
    io $ putStrLn (clientHost ++ " has connected.")
    -- pass hostname and handle to core
    io $ modifyMVar_ clientsMVar' $ return . ((clientHost, clientHandle) :)
    forkMe $ clientRelay (clientHost, clientHandle)
    -- continue loop
    socketlisten sock

-- | Send connected servers and channels their userlists to the client
clientRelay :: CClient -> CMemory ()
clientRelay c@(clientHost, clientHandle) = do
    serverMVar' <- asks (serverMVar . getcMVars)
    servers <- io $ readMVar serverMVar'
    forM_ servers $ \(CServer u h ms) -> do
        forM_ ms $ \m -> do
            let dest = getDestino m
                userlist = getUserlist m
                msg = "getuserlist:" ++ unwords [u, dest, unwords userlist]
            clientWrite c $ T.pack msg

-- |
clientsListen :: [CClient] -> CMemory ()
clientsListen oldSockHs = do
    timeMVar' <- asks (timeMVar . getcMVars)
    clientsMVar' <- asks (clientsMVar . getcMVars)
    textMVar' <- asks (textMVar . getcMVars)
    debug <- asks (verbosityC . getcConfig)
    oldSockHs' <- io $ readMVar clientsMVar'
    forM_ (filter (`notElem` oldSockHs) oldSockHs') $ \h -> do
        forkMe $ clientLoop h
    times <- io $ readMVar timeMVar'
    time <- io $ fmap floor getPOSIXTime
    forM_ times $ \(server, tstamp) -> when (time - tstamp > 240) $ do
        when (debug > 0) $ io $ do
            putStrLn (server ++ " has timed out")
        removeServer server
        rejoinServer server
    ts <- io $ swapMVar textMVar' []
    forM_ ts handleMsg
    io $ threadDelay (10^4)
    clientsListen oldSockHs'

-- TODO: Split this into blocks or something
-- TODO: Respond only to the client requesting the information?
-- | Respond to a core message or pass on an IRC message
handleMsg :: CMessage -> CMemory ()
handleMsg (clientHost, clientText) = do
    debug <- asks (verbosityC . getcConfig)
    servers <- asks (serversC . getcConfig)
    clientsMVar' <- asks (clientsMVar . getcMVars)
    serverMVar' <- asks (serverMVar . getcMVars)
    timeMVar' <- asks (timeMVar . getcMVars)
    ircHandles <- io $ readMVar serverMVar'
    clients <- io $ readMVar clientsMVar'
    let (action, message) = split1 ':' clientText
        send :: Handle -> T.Text -> CMemory (Either SomeException ())
        send h x = io . try $ T.hPutStrLn h x
        cm = listToMaybe $ do
            c@(host, handle) <- clients
            guard $ host == clientHost
            return c
    when (debug > 1) . io . putStrLn $ T.unpack action ++ " == " ++ show ircHandles
    if T.unpack action `elem` map getcServerURL ircHandles then do
        when (debug > 1) . io $ putStrLn "It's an IRC message."
        forM_ ircHandles $ \(CServer server handle' metas) -> do
            when (T.pack server == action) $ do
                e <- send handle' message
                case e of
                    Right _ -> do
                        when (debug > 1) . io $ putStrLn "Successfully sent!"
                        time <- io $ fmap floor getPOSIXTime
                        io $ modifyMVar_ timeMVar' $ return . tupleInject (server, time)
                    Left e' -> when (debug > 0) . io $ print e'
    else if cm /= Nothing then do
        let c = fromJust cm
        when (debug > 1) $ io $ putStrLn "It's a Core message."
        case T.unpack action of
            -- getnick server
            "getnick" -> do
                let server = T.unpack message
                    ownnick = fmap serverNick $ server `getByServerURL` servers
                case ownnick of
                    Just nick -> do
                        clientWrite c . T.pack $ "getnick:" ++  unwords [server, nick]
                    Nothing -> do
                        when (debug > 1) . io $ do
                            putStrLn $ server ++ " not in servers"
            -- "getuserlist server channel
            "getuserlist" -> do
                let msgs = split " " $ T.unpack message
                if length msgs >= 2 then do
                    let (server : channel : xs) = msgs
                        musers = listToMaybe $ do
                            (CServer u h ms) <- ircHandles
                            guard $ server == u
                            return $ do
                                m <- ms
                                guard $ getDestino m == channel
                                getUserlist m
                        jusers = fromJust $ musers <|> Just []
                        users = unwords [ "getuserlist:" ++ server
                                        , channel
                                        , unwords jusers
                                        ]
                    clientWrite c $ T.pack users
                else when (debug > 0) . io $ do
                        putStrLn $ T.unpack action ++ ": Not enough arguments"
            -- getservers
            "getservers" -> do
                let servs = unwords $ map getcServerURL ircHandles
                    servs' = "getservers:" ++ servs
                clientWrite c $ T.pack servs'
            -- quitserver server
            "quitserver" -> removeServer . istrip $ T.unpack message
            -- joinserver server port nick channel1 channel2 ...
            "joinserver" -> do
                let msgs = split " " $ T.unpack message
                if length msgs >= 3 then do
                    let (server : port : nick : xs) = msgs
                        port' = if isNum port
                                    then read port :: Int
                                    else 6667
                        fallbacks = Just $ dServer { serverURL = server
                                                   , serverPort = port'
                                                   , serverNick = nick
                                                   , serverChans = xs
                                                   , serverNSPass = ""
                                                   }
                        server' = fromJust $
                            (server `getByServerURL` servers) <|> fallbacks
                    when (debug > 1) . io $ do
                        putStrLn $ "handleMsg joinserver " ++ serverURL server'
                    cserver <- io $ ircc server'
                    io $ modifyMVar_ serverMVar' (return . (cserver:))
                    ircListen cserver
                else when (debug > 0) . io $ do
                        putStrLn $ T.unpack action ++ " :Not enough arguments"
            _ -> io $ print $ '<' `T.cons` clientText
    else return ()

split1 :: Char -> T.Text -> (T.Text, T.Text)
split1 c t =
    let f = (\(bool, (start, end)) x -> if bool
        then if x == c
            then (False, (start, end))
            else (bool, (start `T.snoc` x, end))
        else (bool, (start, end `T.snoc` x)))
    in snd $ T.foldl f (True, (T.empty, T.empty)) t

elemfst :: Eq a => a -> [(a, b)] -> Bool
elemfst _ [] = False
elemfst a ((x,_):xs) = if a == x then True else elemfst a xs

-- Initialization for clientsLoop which loops clientsListen
clientsInit :: CMemory ()
clientsInit = do
    clientsMVar' <- asks (clientsMVar . getcMVars)
    chs <- io $ readMVar clientsMVar'
    forM_ chs (forkMe . \h -> clientLoop h)
    clientsListen []

clientLoop :: CClient -- specific client handle to read from
           -> CMemory ()
clientLoop ch@(clientHost, clientHandle) = do
    e <- io $ try (T.hGetLine clientHandle) :: CMemory (Either SomeException T.Text)
    case e of
        Right line -> do
            textMVar' <- asks (textMVar . getcMVars)
            io $ modifyMVar_ textMVar' (return . ((clientHost, line) :))
            clientLoop ch
        Left _ -> do
            clientsMVar' <- asks (clientsMVar . getcMVars)
            io $ modifyMVar_ clientsMVar' (return . filter ((/=) clientHandle . snd))

-- TODO: clean up this
-- pass on messages from IRC to the clients in the MVar list
ircListen :: CServer -> CMemory ()
ircListen server@(CServer serverurl serverHandle serverMetas) = do
    debug <- asks (verbosityC . getcConfig)
    servers <- asks (serversC . getcConfig)
    serverMVar' <- asks (serverMVar . getcMVars)
    -- Try to read a message from the handle
    e <- io $ try (T.hGetLine serverHandle) :: CMemory (Either SomeException T.Text)
    when (debug > 0) $ io $ print e
    case e of
        Right line -> do
            forkMe $ parseMsg server line
            ircListen $ CServer serverurl serverHandle []
        -- Reconnect
        Left e -> do
            io $ print e
            when (debug > 1) . io $ putStrLn "Closing handle..."
            io $ hClose serverHandle
            let server' = serverurl `getByServerURL` servers
            case server' of
                Just x -> do
                    when (debug > 1) . io $
                        putStrLn $ "ircListen reconnect " ++ serverURL x
                    newserver <- io $ ircc x
                    -- Function for replacing the old CServer with the new one.
                    let f xs = return . (newserver :) $ do
                        x <- xs
                        guard $ getcHandle x /= getcHandle server
                        return x
                    io $ modifyMVar_ serverMVar' f
                    ircListen newserver
                Nothing -> when (debug > 1) . io $ do
                    putStrLn $ "Server `" ++ serverurl ++ "' not in `servers'"

parseMsg :: CServer -> T.Text -> CMemory ()
parseMsg (CServer serverurl serverHandle serverMetas) line = do
    debug <- asks (verbosityC . getcConfig)
    servers <- asks (serversC . getcConfig)
    timeMVar' <- asks (timeMVar . getcMVars)
    clientsMVar' <- asks (clientsMVar . getcMVars)
    let sFull :: [T.Text]
        sFull = (== ':') `T.split` line
        sMsg  = (T.pack ":") `T.intercalate` drop 2 sFull
        sArgs = T.split (`elem` "!@ ") . T.concat . take 1 $ drop 1 sFull
    time <- io $ fmap floor getPOSIXTime
    io $ modifyMVar_ timeMVar' $ return . tupleInject (serverurl, time)
    clientsWrite $ T.pack serverurl `T.append` line
    case () of
      _ | isPing line -> do
            io . T.hPutStrLn serverHandle $ (T.pack "PONG") `T.append` T.drop 4 line
        | isStatus line "003" -> do
            when (debug > 1) $ do
                io $ putStrLn "Server welcome message, joining channels."
            let server' = findServer servers serverurl
            when (isJust server') $ do
                joinChannels serverHandle $ fromJust server'
                nickservRegister serverHandle $ fromJust server'
        | isStatus line "353" -> do -- add userlist
            let dest = T.unpack $ sArgs !! 4
                users = words $ T.unpack $ sMsg
                users' = map (dropWhile (`elem` "~@%&+")) users
            mapUserlist $ \xs -> do
                cs@(CServer u h ms) <- xs
                if serverurl == u then
                    let meta = emptyMeta { getDestino = dest
                                         , getUserlist = users'
                                         }
                        ms' = meta : do
                            m <- ms
                            guard $ dest /= getDestino m
                            return m
                    in return $ CServer u h ms'
                else return cs
        | isStatus line "JOIN" -> do -- add user to userlist
            let dest = T.unpack $ sMsg
                user = T.unpack $ sArgs !! 0
            mapUserlist $ \xs -> do
                cs@(CServer u h ms) <- xs
                if serverurl == u then
                    let ms' = do
                            m <- ms
                            if dest == getDestino m
                                then return $ mapMetaUserlist (user :) m
                                else return m
                    in return $ CServer u h ms'
                else return cs
        | isStatus line "QUIT" -> do -- remove user from all userlists on server
            let user = T.unpack $ sArgs !! 0
            mapUserlist $ \xs -> do
                cs@(CServer u h ms) <- xs
                if serverurl == u then
                    let ms' = do
                            m <- ms
                            let f = filter (/= user)
                            return $ mapMetaUserlist f m
                    in return $ CServer u h ms'
                else return cs
        | isStatus line "PART" -> do -- remove user from userlist
            let dest = T.unpack $ sArgs !! 4
                user = T.unpack $ sArgs !! 0
            mapUserlist $ \xs -> do
                cs@(CServer u h ms) <- xs
                if serverurl == u then
                    let ms' = do
                            m <- ms
                            let f = filter (/= user)
                            if dest == getDestino m
                                then return $ mapMetaUserlist f m
                                else return m
                    in return $ CServer u h ms'
                else return cs
        | isStatus line "KICK" -> do -- remove user from userlist
            let dest = T.unpack $ sArgs !! 4
                user = T.unpack $ sArgs !! 5
            mapUserlist $ \xs -> do
                cs@(CServer u h ms) <- xs
                if serverurl == u then
                    let ms' = do
                            m <- ms
                            let f = filter (/= user)
                            if dest == getDestino m
                                then return $ mapMetaUserlist f m
                                else return m
                    in return $ CServer u h ms'
                else return  cs
        | isStatus line "NICK" -> do -- change nick in userlist
            let user = T.unpack $ sArgs !! 0
                user2 = T.unpack $ sArgs !! 4
            mapUserlist $ \xs -> do
                cs@(CServer u h ms) <- xs
                if serverurl == u then
                    let ms' = do
                            m <- ms
                            let f = (user2 :) . filter (/= user)
                            return $ mapMetaUserlist f m
                    in return $ CServer u h ms'
                else return cs
        | isStatus line "433" -> do
            let nick' = (++ "`") . serverNick <$> findServer servers serverurl
            when (isJust nick') $ do
                io . T.hPutStrLn serverHandle $
                    T.unwords [T.pack "NICK", T.pack $ fromJust nick']
        | otherwise -> return ()
  where isPing :: T.Text -> Bool
        isPing x = T.take 4 x == (T.pack "PING")
        isStatus :: T.Text -> String -> Bool
        isStatus x n =
            let xs = T.words x
                status = T.pack n
            in if length xs >= 2
                then xs !! 1 == status
                else False
        isJust (Just _) = True
        isJust _ = False

-- connect to an IRC server and return the handle
ircc :: Server -> IO CServer
ircc (Server port server chans nick nsPW _ _ _) = do
    h <- connectTo server . PortNumber $ fromIntegral port
    hSetEncoding h utf8
    hSetBuffering h LineBuffering
    hSetNewlineMode h (NewlineMode CRLF CRLF)
    forM_ [nick', user'] (write h . T.pack)

    return $ CServer server h []
  where nick' = "NICK " ++ nick
        user' = "USER " ++ nick ++ " 0 * :" ++ nick

joinChannels :: Handle -> Server -> CMemory ()
joinChannels h (Server _ serverurl chans _ _ _ _ _) = do
    let chans' = map ("JOIN " ++) chans
    forM_ chans' $ io . write h . T.pack

nickservRegister :: Handle -> Server -> CMemory ()
nickservRegister h (Server _ _ _ _ nsPW _ _ _) = do
    debug <- asks (verbosityC . getcConfig)
    unless (null nsPW) . io .  write h . T.pack $ unwords [identify, nsPW]
    if debug > 1
        then io $ putStrLn $ unwords [identify, nsPW]
        else io $ putStrLn $ ">" ++ unwords [identify, take (length nsPW) censor]
  where identify = "PRIVMSG NickServ :IDENTIFY"
        censor = cycle "*"

mapUserlist :: ([CServer] -> [CServer]) -> CMemory ()
mapUserlist f = do
    serverMVar' <- asks (serverMVar . getcMVars)
    io . modifyMVar_ serverMVar' $ return . f

-- create some core data, connect to IRC servers and create the server socket
main :: IO ()
main = do
    let debug = verbosityC config
        servers = serversC config
    -- IRC handles
    hs <- forM servers ircc
    when (debug > 1) $ print hs
    -- list of clients connected to the socket
    sockMVar <- newMVar []
    --  MVar with Text input from clients connected to the socket.
    inputMVar <- newMVar []
    -- MVar with current IRC servers.
    serverMVar <- newMVar hs
    -- generate timestamp
    time <- fmap floor getPOSIXTime
    timeMVar <- newMVar $ zip (map getcServerURL hs) (cycle [time])
    let mvarConfig = MVarConfig { getcChannels = []
                                , getcMVars = MVars { clientsMVar = sockMVar
                                                    , textMVar = inputMVar
                                                    , serverMVar = serverMVar
                                                    , timeMVar = timeMVar
                                                    }
                                , getcConfig = config
                                }
        run f = runReaderT f mvarConfig
    -- create socket that clients connect to
    socket <- listenOn (PortNumber $ fromIntegral 3737)
    run . void . forkMe $ socketlisten socket
    forM_ hs (\h -> run . forkMe $ ircListen h)
    run . void . forkMe $ clientsInit
    run . forever $ userInput

-- get direct input prefixed with the server URL and send, for example:
-- irc.freenode.net:PRIVMSG #kawaiibot :This is a nice message!
userInput :: CMemory ()
userInput = do
    debug <- asks (verbosityC . getcConfig)
    serverMVar' <- asks (serverMVar . getcMVars)
    hs <- io $ readMVar serverMVar'
    line <- io $ T.getLine
    forM_ hs $ \(CServer s h m) -> do
        let server = T.takeWhile (/= ':') line
        when (server == T.pack s) $ do
            io $ T.hPutStrLn h (T.tail $ T.dropWhile (/= ':') line)
            when (debug > 1) . io $ putStrLn "Successfully sent!"

-- hClose and remove a server tuple from the IRC server MVar
removeServer :: String -> CMemory ()
removeServer server = do
    serverMVar' <- asks (serverMVar . getcMVars)
    timeMVar' <- asks (timeMVar . getcMVars)
    let f = \(CServer server' handle metas) -> if server == server'
                then do
                    io $ hClose handle
                    return False
                else do
                    return True
    io $ modifyMVar_ serverMVar' $ filterM f
    io $ modifyMVar_ timeMVar' $ filterM (return . (/= server) . fst)

rejoinServer :: String -> CMemory ()
rejoinServer server = do
    debug <- asks (verbosityC . getcConfig)
    serverMVar' <- asks (serverMVar . getcMVars)
    timeMVar' <- asks (timeMVar . getcMVars)
    mserver <- asks (getByServerURL server . serversC . getcConfig)
    case mserver of
        Just server' -> do
            when (debug > 1) . io $ do
                putStrLn $ "rejoinServer " ++ server
            cserver <- io $ ircc server'

            time <- io $ fmap floor getPOSIXTime
            io $ modifyMVar_ timeMVar' $ return . tupleInject (server, time)

            io $ modifyMVar_ serverMVar' (return . (cserver:))
            ircListen cserver
        Nothing -> io . putStrLn $ unwords [ "Rejoin:"
                                           , server
                                           , "not in servers"
                                           ]

io :: MonadIO m => IO a -> m a
io = liftIO
