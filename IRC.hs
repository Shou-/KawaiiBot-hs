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

module IRC where


import Bot
import Types
import Utils

import Control.Concurrent
import Control.Exception
import Control.Monad hiding (join)
import Control.Monad.Reader hiding (join)
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX
import Data.Maybe (fromJust, listToMaybe)
import Data.String.Utils (split, join, strip)
import Network
import System.IO
import System.Random (randomRIO)


-- Writes a String to a handle and prints the message
write :: Handle -> Meta -> String -> IO ()
write h meta str = do
    let server = getServer meta
        message = server ++ ":" ++ str
    e <- liftIO $ try (hPutStrLn h message) :: IO (Either SomeException ())
    case e of
        Right _ -> liftIO . putStrLn $ '>' : message
        Left e -> liftIO $ print e

eventInit :: Event -> Memory Event
eventInit (Event f r ti c s te) = do
    time <- liftIO $ fmap realToFrac getPOSIXTime
    time' <- liftIO $ fmap (time +) r
    return $ Event f r time' c s te

event :: Handle -> Memory ()
event h = do
    events <- asks (eventsC . getConfig)
    currentTime <- liftIO $ fmap realToFrac getPOSIXTime
    events' <- forM events $ \event -> do
        let bool = currentTime >= eventTime event
        cbool <- liftIO $ chance (eventChance event)
        case () of
          _ | bool && cbool -> do
                let servers :: [Server]
                    servers = eventServers event
                    metas :: [Meta]
                    metas = concat . (`map` servers) $
                                \(Server _ server channels _ _ _ _) ->
                                    (`map` channels) $ \channel ->
                                        Meta channel "Anon" "" "" [] server ""
                forM_ metas $ \meta -> do
                    text <- local (injectMeta meta) $ eventFunc event
                    unless (null text) $ do
                        liftIO . write h meta $ genPrivmsg meta text
                eventInit event
            | not bool -> return event
            | not cbool -> eventInit event
    liftIO . threadDelay $ 10^6
    local (modConfig $ injectEvents events') $ event h
  where genPrivmsg (Meta dest _ _ _ _ _ _) t = "PRIVMSG " ++ dest ++ " :" ++ t
        chance :: Double -> IO Bool
        chance n = do
            let n' = if n > 1.0 then 1.0 else n
            i <- randomRIO (0.0, 1.0)
            return (n >= i)

listenLoop :: Handle -> MVar [String] -> Memory ()
listenLoop h eventMVar = do
    s <- liftIO $ hGetLine h
    parse h s
    listenLoop h eventMVar

parse :: Handle -- IRC server handle
      -> String -- message received
      -> Memory ()
parse h bs = do
    meta <- asks getMeta
    let nick = getUsernick meta
        sFull :: [String]
        sFull = ":" `split` bs
        sMsg  = ":" `join` drop 2 sFull
        sArgs = map strip . splits "!@ " . concat $ take 1 $ drop 1 sFull
        meta' = sFull !! 0 `injectServ` meta
    liftIO . putStrLn $ show sArgs ++ ' ' : sMsg
    if sFull !! 0 `elem` coreArgs
        then return () -- do something you loser
        else local (injectMeta meta') $ parseMsg h (sArgs, sMsg)
  where parseMsg :: Handle -> ([String], String) -> Memory ()
        parseMsg h (args, msg)
            | args !! 3 == "PRIVMSG" = do -- Interpret message and respond if anything's returned from `msgInterpret'
                meta <- asks getMeta
                let nick = args !! 0
                    name = args !! 1
                    host = args !! 2
                    act = args !! 3
                    dest = args !! 4
                    channels = getChannels meta
                    serverurl = getServer meta
                    ownnick = getOwnNick meta

                    meta' = Meta dest nick name host channels serverurl ownnick
                local (injectMeta meta') $ do
                    meta <- asks getMeta
                    urlFetching <- asks (urlFetchingC . getConfig)
                    msgLogging <- asks (msgLoggingC . getConfig)

                    when urlFetching . void . liftIO . forkIO $ do -- URL fetching
                        let urls = filter (isPrefixOf "http") $ words msg
                        forM_ urls $ \url -> do
                            title' <- title url :: IO String
                            let message = "PRIVMSG " ++ dest ++ " :\ETX5Title\ETX: " ++ title'
                            unless (null title') $ write h meta message
                    post <- msgInterpret msg
                    let mAct = if isChannelMsg post
                            then "PRIVMSG " ++ dest
                            else "PRIVMSG " ++ nick
                        msg' = fromMsg post

                    unless (null msg') . liftIO . write h meta $ mAct ++ " :" ++ msg'

                    logsPath <- asks (logsPathC . getConfig)
                    verbosity <- asks (verbosityC . getConfig)
                    liftIO . when msgLogging $ do -- logging
                        e <- try (do
                            let path = logsPath ++ serverurl ++ " " ++ dest
                            appendFile path $ show (args, msg) ++ "\n") :: IO (Either SomeException ())
                        case e of
                            Right _ -> return ()
                            Left e -> do
                                when (verbosity > 0) $ print e

                    return ()
            | args !! 3 == "INVITE" = do -- Join a channel on invite
                meta <- asks getMeta
                let nick = args !! 0
                    name = args !! 1
                    host = args !! 2
                    act = args !! 3
                    dest = args !! 4
                    channels = getChannels meta
                    serverurl = getServer meta
                    ownnick = getOwnNick meta

                    meta' = Meta dest nick name host channels serverurl ownnick

                -- check this channel against a list of banned ones or something
                --liftIO . write h meta' $ "JOIN " ++ msg
                liftIO . write h meta' $ "PRIVMSG " ++ nick ++ " :Invites are currently disabled due to KawaaiiBot's incompleteness."
                return ()
            | args !! 3 == "KICK" = do 
                let dest = args !! 4

                return ()
            | args !! 3 == "PART" = do
                let nick = args !! 0
                    dest = args !! 4

                return ()
            | args !! 3 == "JOIN" = do
                let nick = args !! 0
                    dest = msg

                -- get :reminder variable for person joining
                return ()
            | args !! 3 == "QUIT" = do 
                let nick = args !! 0

                return ()
            | args !! 3 == "NICK" = do 
                let nick = args !! 0

                return ()
            | args !! 3 == "MODE" = do 
                let nick = args !! 0
                    dest = args !! 4
                    mode = args !! 5

                return ()
            | args !! 1 == "353" = do -- Receive nick list
                let dest = args !! 4 -- use this stuff to handle userlists later
                    --f = \_ -> map (\x'@(x:xs) -> if x `elem` ['~', '&', '@', '%', '+'] then (x, xs) else (' ', x')) $ split " " msg
                    --meta' = f `modUserlist` meta -- meta doesn't hold userlists anymore

                return ()
            | otherwise = do -- Fallback
                return ()
        coreArgs = ["getservers", "getnick", "serverjoin", "serverquit"]

-- Connects to an IRC server
serverConnect :: Memory ()
serverConnect = do
    h <- liftIO $ do
        h <- connectTo "localhost" (PortNumber $ fromIntegral 3737)
        hSetEncoding h utf8
        hSetBuffering h LineBuffering
        hSetNewlineMode h (NewlineMode CRLF CRLF)
        forkIO . forever $ getLine >>= \s -> unless (null s) $ hPutStrLn h s
        return h
    eventMVar <- liftIO $ newMVar []
    events <- asks (eventsC . getConfig)
    events' <- mapM eventInit events
    local (modConfig $ injectEvents events') (forkMe $ event h)
    listenLoop h eventMVar
