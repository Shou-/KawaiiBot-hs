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

import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.List
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network
import System.Directory
import System.IO

-- TODO:
---- Show disconnections instantly
---- If an IRC handle is closed, reconnect. Probably need another MVar.
------ Work has been started. Make sure handle actions are within `try' functions.
---- Additional internal functions available to the clients:
------ `connect irc.freenode.net' to connect to an IRC server. Disconnect too.
------ `getservers' to make it output which IRC servers are connected
---- Register with nickserv
------ Need password from destkop.

type Client = (String, Handle)
type Server = (String, Handle)
type Message = (String, T.Text)
type Timestamp = (String, Int)

-- Print debug messages
debug :: Bool
debug = True

-- Simple function that writes to a handle
-- and prints the bytestring
write :: Handle -> T.Text -> IO ()
write h bs = T.hPutStrLn h bs >> T.putStrLn ('>' `T.cons` bs)

-- Write to all open client handles
-- Also return new list based on whether client is connected or not
clientsWrite :: MVar [Client] -> T.Text -> IO ()
clientsWrite mvar line = do
    when debug $ putStrLn "clientsWrite init..."
    hs <- takeMVar mvar
    when debug $ putStrLn "clientsWrite after takeMVar"
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
socketlisten :: MVar [Client] -> Socket -> IO ()
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

clientsLoop :: MVar [Server]
            -> MVar [Timestamp]
            -> [Client]
            -> MVar [Client]
            -> MVar [Message]
            -> IO ()
clientsLoop i p o s c =
    clientsListen i p o s c >>= \(i', p', o', s', c') -> clientsLoop i' p' o' s' c'

-- 
clientsListen :: MVar [Server] -- irc handles, passing on Text from clients
              -> MVar [Timestamp] -- POSIX timestamp of latest message
              -> [Client] -- old handles for writing to clients
              -> MVar [Client] -- new handles for writing to clients
              -> MVar [Message] -- Text received from client handles
              -> IO (MVar [Server], MVar [Timestamp], [Client], MVar [Client], MVar [Message])
clientsListen serverMVar timeMVar oldSockHs sockMVar textMVar = do
    oldSockHs' <- readMVar sockMVar
    forM_ (filter (`notElem` oldSockHs) oldSockHs') $ \h -> do
        forkIO $ clientLoop h sockMVar textMVar
    times <- readMVar timeMVar
    time <- fmap floor getPOSIXTime
    forM_ times $ \(server, tstamp) -> when (time - tstamp > 240) $ do
        when debug $ putStrLn (server ++ " has timed out")
        modifyMVar_ serverMVar $ \servers -> do
            let f = \(server', handle) -> if server == server'
                        then hClose handle >> return False
                        else return True
            filterM f servers
    ts <- swapMVar textMVar []
    forM_ ts handleMsg
    threadDelay 10
    return (serverMVar, timeMVar, oldSockHs', sockMVar, textMVar)
  where handleMsg :: Message -> IO ()
        handleMsg (_, clientText) = do
            ircHandles <- readMVar serverMVar
            let (action, message) = split1 ':' clientText
                send :: Handle -> T.Text -> IO (Either SomeException ())
                send h x = try $ T.hPutStrLn h x
            if T.unpack action `elemfst` ircHandles
                then do
                    forM_ ircHandles $ \(server, handle') -> do
                        when (T.pack server == action) $ do
                            e <- send handle' message
                            when debug $ case e of
                                Right _ -> putStrLn "Successfully sent!"
                                Left e' -> print e'
                else do
                    case T.unpack action of
                        "getnick" -> return ()
                        "getservers" -> do
                            let servs = intercalate " " $ map fst ircHandles
                                servs' = "getservers:" ++ servs
                            clients <- readMVar sockMVar
                            forM_ clients $ \(_, h) -> send h $ T.pack servs'
                        "getchannels" -> return ()
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
clientsInit :: MVar [Server]
            -> MVar [Timestamp]
            -> MVar [Client]
            -> MVar [Message]
            -> IO ()
clientsInit serverMVar timeMVar sockMVar textMVar = do
    chs <- readMVar sockMVar
    forM_ chs (forkIO . \h -> clientLoop h sockMVar textMVar)
    clientsLoop serverMVar timeMVar [] sockMVar textMVar

clientLoop :: Client -- specific handle to read from
           -> MVar [Client] -- MVar with client handles
           -> MVar [Message] -- MVar to append Text gotten from handle
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
ircListen :: Server -> MVar [Server] -> MVar [Timestamp] -> MVar [Client] -> IO ()
ircListen server@(serverURL, serverHandle) serverMVar timeMVar clientMVar = do
    e <- try (T.hGetLine serverHandle) :: IO (Either SomeException T.Text)
    when debug $ print e
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
            when debug $ putStrLn "Closing handle..."
            hClose serverHandle
            server' <- ircc (serverURL, 6667)
            modifyMVar_ serverMVar (return . (server' :) . filter (/= server))
  where isPing :: T.Text -> Bool
        isPing x = T.take 4 x == (T.pack "PING")

-- connect to an IRC server and return the handle
-- TODO: read this from a config file or something, prferably within main
ircc :: (String, Int) -> IO Server
ircc (server, port) = do
    h <- connectTo server . PortNumber $ fromIntegral port
    hSetEncoding h utf8
    hSetBuffering h LineBuffering
    hSetNewlineMode h (NewlineMode CRLF CRLF)
    forM_ (nick : user : chans) (write h . T.pack)
    return (server, h)
  where nick = "NICK KawaiiBot"
        user = "USER KawaiiBot 0 * :KawaiiBot"
        chans = map ("JOIN " ++) ["#KawaiiBot"]
{-
-- What should the config look like?
getConfig :: IO Something
getConfig = do
    home <- getHomeDirectory
    e <- try (readFile $ home ++ "/.kawaiibot-core.conf") :: IO (Either SomeException String)
    case e of
        Right file ->
        Left _ -> 
-}
-- create some core data, connect to IRC servers and create the server socket
main :: IO ()
main = do
    -- IRC handles
    hs <- forM servers ircc
    when debug $ print hs
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
  where servers = [("irc.rizon.net", 6667)]

startServer :: IO ()
startServer = main

--serverMessage :: MVar [Server] -> IO ()
--serverMessage ::

-- get direct input prefixed with the server URL and send, for example:
-- irc.freenode.net:PRIVMSG #kawaiibot :This is a nice message!
userInput :: MVar [Server] -> IO ()
userInput serverMVar = do
    hs <- readMVar serverMVar
    line <- T.getLine
    forM_ hs $ \(s, h) -> do
        let server = T.takeWhile (/= ':') line
        when (server == T.pack s) $ do
            T.hPutStrLn h (T.tail $ T.dropWhile (/= ':') line)
            when debug $ putStrLn "Successfully sent!"
