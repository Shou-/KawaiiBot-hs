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


import Utils
import Types
import Config

import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.List
import Data.Maybe (fromJust)
import Data.String.Utils (split)
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network
import System.Directory
import System.IO

-- lazy piece of shit
---- Pass `config' around to be able to modify it and thus the state of the software.
debug = verbosityC config
servers = serversC config

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
---- Lower the CPU usage.
------ See: clientsListen
------ CPU usage used to be 0% with `threadDelay 10', see if we can lower it again.
---- JOINs are ignored if sent too early. Wait for the message with status code `001'.

type CClient = (String, Handle)
type CServer = (String, Handle)
type CMessage = (String, T.Text)
type CTimestamp = (String, Int)

-- Simple function that writes to a handle
-- and prints the bytestring
write :: Handle -> T.Text -> IO ()
write h bs = T.hPutStrLn h bs >> T.putStrLn ('>' `T.cons` bs)

-- Write to all open client handles
-- Also return new list based on whether client is connected or not
clientsWrite :: MVar [CClient] -> T.Text -> IO ()
clientsWrite mvar line = do
    when (debug > 1) $ putStrLn "clientsWrite init..."
    hs <- takeMVar mvar
    when (debug > 1) $ putStrLn "clientsWrite after takeMVar"
    mhs <- forM hs $ \(clientHost, clientHandle) -> do
        -- check if handle is still open
        e <- try (T.hPutStrLn clientHandle line >> return clientHandle) :: IO (Either SomeException Handle)
        -- if it didn't return an exception, continue
        case e of
            -- return Just the host and handle
            Right clientHandle' ->  return $ Just (clientHost, clientHandle')
            -- print a disconnection message and return Nothing
            Left _ -> putStrLn (clientHost ++ " has disconnected.") >> return Nothing
    T.putStrLn $ '>' `T.cons` line
    putMVar mvar $ foldr (\x acc -> if isJust x then fromJust x : acc else acc) [] mhs
  where isJust (Just _) = True
        isJust _ = False

-- loop that handles client connections and adds them to the MVar list
socketlisten :: MVar [CClient] -> Socket -> IO ()
socketlisten mvar sock = do
    -- accept a socket
    (clientHandle, clientHost, _) <- accept sock
    -- set handle character encoding to utf8
    hSetEncoding clientHandle utf8
    -- line buffering so messages show up instantly
    hSetBuffering clientHandle LineBuffering
    -- newline mode to \r\n
    hSetNewlineMode clientHandle (NewlineMode CRLF CRLF)
    putStrLn (clientHost ++ " has connected.")
    -- pass hostname and handle to core
    modifyMVar_ mvar $ return . ((clientHost, clientHandle) :)
    -- continue loop
    socketlisten mvar sock

clientsLoop :: MVar [CServer]
            -> MVar [CTimestamp]
            -> [CClient]
            -> MVar [CClient]
            -> MVar [CMessage]
            -> IO ()
clientsLoop i p o s c =
    clientsListen i p o s c >>= \(i', p', o', s', c') -> clientsLoop i' p' o' s' c'

-- 
clientsListen :: MVar [CServer] -- irc handles, passing on Text from clients
              -> MVar [CTimestamp] -- POSIX timestamp of latest message
              -> [CClient] -- old handles for writing to clients
              -> MVar [CClient] -- new handles for writing to clients
              -> MVar [CMessage] -- Text received from client handles
              -> IO (MVar [CServer], MVar [CTimestamp], [CClient], MVar [CClient], MVar [CMessage])
clientsListen serverMVar timeMVar oldSockHs sockMVar textMVar = do
    oldSockHs' <- readMVar sockMVar
    forM_ (filter (`notElem` oldSockHs) oldSockHs') $ \h -> do
        forkIO $ clientLoop h sockMVar textMVar
    times <- readMVar timeMVar
    time <- fmap floor getPOSIXTime
    forM_ times $ \(server, tstamp) -> when (time - tstamp > 240) $ do
        when (debug > 0) $ putStrLn (server ++ " has timed out")
        removeServer serverMVar server
    ts <- swapMVar textMVar []
    forM_ ts handleMsg
    threadDelay (10^4)
    return (serverMVar, timeMVar, oldSockHs', sockMVar, textMVar)
    -- when a where variable gets this big you break it off
    -- or at least put blocks of it into bottom level functions
  where handleMsg :: CMessage -> IO ()
        handleMsg (_, clientText) = do
            ircHandles <- readMVar serverMVar
            let (action, message) = split1 ':' clientText
                send :: Handle -> T.Text -> IO (Either SomeException ())
                send h x = try $ T.hPutStrLn h x
            when (debug > 1) . putStrLn $ T.unpack action ++ " == " ++ show ircHandles
            if T.unpack action `elemfst` ircHandles
                then do
                    when (debug > 1) $ putStrLn "It's an IRC message."
                    forM_ ircHandles $ \(server, handle') -> do
                        when (T.pack server == action) $ do
                            e <- send handle' message
                            case e of
                                Right _ -> when (debug > 1) $ putStrLn "Successfully sent!"
                                Left e' -> when (debug > 0) $ print e'
                else do
                    when (debug > 1) $ putStrLn "It's a Core message."
                    case T.unpack action of
                        "getnick" -> do
                            let ownnick = fmap serverNick $ servers `getByServerURL` T.unpack message
                            case ownnick of
                                Just nick -> do
                                    clients <- readMVar sockMVar
                                    forM_ clients $ \(_, h) -> send h . T.pack $ "getnick:" ++ nick -- ONE WORD: THE FORCED INDENTATION OF THE CODE, THREAD OVER
                                Nothing -> do
                                    when (debug > 1) . putStrLn $ "Server `" ++ T.unpack message ++ "' not in `servers'"
                        "getservers" -> do
                            let servs = intercalate " " $ map fst ircHandles
                                servs' = "getservers:" ++ servs
                            clients <- readMVar sockMVar
                            forM_ clients $ \(_, h) -> send h $ T.pack servs'
                        "serverquit" -> removeServer serverMVar . istrip $ T.unpack message
                        "serverjoin" -> do
                            let msgs = split " " $ T.unpack message
                            if length msgs >= 3
                                then do
                                    let (server : port : nick : xs) = msgs
                                        port' = if isNum port
                                                    then read port :: Int
                                                    else 6667
                                        server' = defaultServer { serverURL = server
                                                                , serverPort = port'
                                                                , serverNick = nick
                                                                , serverChans = xs
                                                                , serverNSPass = ""
                                                                }
                                    cserver <- ircc server'
                                    modifyMVar_ serverMVar (return . (cserver:))
                                else when (debug > 0) $ putStrLn "Not enough arguments"
                        _ -> print $ '<' `T.cons` clientText
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
clientsInit :: MVar [CServer]
            -> MVar [CTimestamp]
            -> MVar [CClient]
            -> MVar [CMessage]
            -> IO ()
clientsInit serverMVar timeMVar sockMVar textMVar = do
    chs <- readMVar sockMVar
    forM_ chs (forkIO . \h -> clientLoop h sockMVar textMVar)
    clientsLoop serverMVar timeMVar [] sockMVar textMVar

clientLoop :: CClient -- specific client handle to read from
           -> MVar [CClient] -- MVar with client handles
           -> MVar [CMessage] -- MVar to append Text gotten from handle
           -> IO ()
clientLoop ch@(clientHost, clientHandle) sockMVar textMVar = do
    e <- try (T.hGetLine clientHandle) :: IO (Either SomeException T.Text)
    case e of
        Right line -> do
            modifyMVar_ textMVar (return . ((clientHost, line) :))
            clientLoop ch sockMVar textMVar
        Left _ -> do
            modifyMVar_ sockMVar (return . filter ((/=) clientHandle . snd))

-- pass on messages from IRC to the clients in the MVar list
ircListen :: CServer -> MVar [CServer] -> MVar [CTimestamp] -> MVar [CClient] -> IO ()
ircListen server@(serverURL, serverHandle) serverMVar timeMVar clientMVar = do
    e <- try (T.hGetLine serverHandle) :: IO (Either SomeException T.Text)
    when (debug > 0) $ print e
    case e of
        Right line -> do
            time <- fmap floor getPOSIXTime
            modifyMVar_ timeMVar $ return . tupleInject (serverURL, time)
            if isPing line
                then T.hPutStrLn serverHandle $ (T.pack "PONG") `T.append` T.drop 4 line
                else clientsWrite clientMVar $ T.pack serverURL `T.append` line
            ircListen (serverURL, serverHandle) serverMVar timeMVar clientMVar
        Left e -> do
            print e
            when (debug > 1) $ putStrLn "Closing handle..."
            hClose serverHandle
            let server' = servers `getByServerURL` serverURL
            case server' of
                Just x -> do
                    newserver <- ircc x
                    modifyMVar_ serverMVar (return . (newserver :) . filter (/= server))
                -- ABRA KADABRA! ヽ（ ﾟヮﾟ）ﾉ.・ﾟ*｡・+☆┳━┳
                Nothing -> when (debug > 1) . putStrLn $ "Server `" ++ serverURL ++ "' not in `servers'"
  where isPing :: T.Text -> Bool
        isPing x = T.take 4 x == (T.pack "PING")

-- connect to an IRC server and return the handle
ircc :: Server -> IO CServer
ircc (Server port server chans nick nsPW _ _) = do
    h <- connectTo server . PortNumber $ fromIntegral port
    hSetEncoding h utf8
    hSetBuffering h LineBuffering
    hSetNewlineMode h (NewlineMode CRLF CRLF)
    forM_ (nick' : user' : chans') (write h . T.pack)
    unless (null nsPW) $ do
        T.hPutStrLn h . T.pack $ unwords [identify, nsPW]
        if debug > 1
            then putStrLn $ unwords [identify, nsPW]
            else putStrLn $ ">" ++ unwords [identify, take (length nsPW) censor]
    return (server, h)
  where nick' = "NICK " ++ nick
        user' = "USER " ++ nick ++ " 0 * :" ++ nick
        chans' = map ("JOIN " ++) chans
        identify = "PRIVMSG NickServ :IDENTIFY"
        censor = cycle "*"

-- create some core data, connect to IRC servers and create the server socket
main :: IO ()
main = do
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
    timeMVar <- newMVar $ zip (map fst hs) (cycle [time])
    -- create socket that clients connect to
    socket <- listenOn (PortNumber $ fromIntegral 3737)
    _ <- forkIO $ socketlisten sockMVar socket
    forM_ hs (\h -> forkIO $ ircListen h serverMVar timeMVar sockMVar)
    _ <- forkIO $ clientsInit serverMVar timeMVar sockMVar inputMVar
    forever $ userInput serverMVar

-- get direct input prefixed with the server URL and send, for example:
-- irc.freenode.net:PRIVMSG #kawaiibot :This is a nice message!
userInput :: MVar [CServer] -> IO ()
userInput serverMVar = do
    hs <- readMVar serverMVar
    line <- T.getLine
    forM_ hs $ \(s, h) -> do
        let server = T.takeWhile (/= ':') line
        when (server == T.pack s) $ do
            T.hPutStrLn h (T.tail $ T.dropWhile (/= ':') line)
            when (debug > 1) $ putStrLn "Successfully sent!"

-- hClose and remove a server tuple from the IRC server MVar
removeServer :: MVar [CServer] -> String -> IO ()
removeServer serverMVar server = do
    let f = \(server', handle) -> if server == server'
                then hClose handle >> return False
                else return True
    modifyMVar_ serverMVar $ filterM f
