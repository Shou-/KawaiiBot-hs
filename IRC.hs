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

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad hiding (join)
import qualified Control.Monad as M
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
write :: Handle -> String -> Memory ()
write h str = do
    meta <- asks getMeta
    let server = getServer meta
        message = server ++ ":" ++ str
    e <- liftIO $ try (hPutStrLn h message) :: Memory (Either SomeException ())
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
            temp = eventTemp event
        cbool <- liftIO $ chance (eventChance event)
        case () of
          _ | bool && cbool -> do
                let servers :: [Server]
                    servers = eventServers event
                    metas :: [Meta]
                    metas = concat . (`map` servers) $
                        \(Server _ server _ _ _ channels _) ->
                            case channels of
                                Blacklist chans -> []
                                Whitelist chans ->
                                    (`map` chans) $ \chan ->
                                        Meta chan "Anon" "" "" [] server temp
                forM_ metas $ \meta -> do
                    text <- local (injectMeta meta) $ eventFunc event
                    unless (null text) $ do
                        local (injectMeta meta) $ do
                            write h $ genPrivmsg meta text
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
            | isCmd args 3 "PRIVMSG" = do -- Interpret message and respond if anything's returned from `parser'
                meta <- asks getMeta
                let nick = args !! 0
                    name = args !! 1
                    host = args !! 2
                    act = args !! 3
                    dest = args !! 4
                    channels = getChannels meta
                    serverurl = getServer meta
                    temp = getTemp meta

                    meta' = Meta dest nick name host channels serverurl temp

                local (injectMeta meta') $ do
                    meta <- asks getMeta
                    titleFetching <- getFunc allowTitle
                    msgLogging <- asks (msgLoggingC . getConfig)

                    when titleFetching . void . forkMe $ do -- URL fetching
                        let urls = filter (isPrefixOf "http") $ words msg
                        forM_ urls $ \url -> do
                            title' <- fmap fromMsg $ title url
                            let message = "PRIVMSG " ++ dest ++ " :\ETX5Title\ETX: " ++ title'
                            unless (null title') $ write h message
                    post <- parser msg
                    let mAct = if isChannelMsg post
                            then "PRIVMSG " ++ dest
                            else "PRIVMSG " ++ nick
                        msg' = fromMsg post

                    unless (null msg') . write h $ mAct ++ " :" ++ msg'

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
            | isCmd args 3 "INVITE" = do -- Join a channel on invite
                meta <- asks getMeta
                let nick = args !! 0
                    name = args !! 1
                    host = args !! 2
                    act = args !! 3
                    dest = args !! 4
                    channels = getChannels meta
                    serverurl = getServer meta
                    temp = getTemp meta

                    meta' = Meta dest nick name host channels serverurl temp

                -- check this channel against a list of banned ones or something
                allowedChans <- asks (fmap allowedChannels . (`findServer` serverurl) . serversC . getConfig)
                case allowedChans of
                    Just (Blacklist xs) -> do
                        if msg `elem` xs then do
                            write h $ "JOIN " ++ msg
                        else do
                            write h $ "PRIVMSG " ++ dest ++ " :Your channel is blacklisted."
                    Just (Whitelist xs) -> do
                        if msg `elem` xs then do
                            write h $ "JOIN " ++ msg
                        else do
                            write h $ "PRIVMSG " ++ dest ++ " :Your channel is not whitelisted."
                    Nothing -> return ()
                return ()
            | isCmd args 3 "KICK" = do 
                let dest = args !! 4

                return ()
            | isCmd args 3 "PART" = do
                let nick = args !! 0
                    dest = args !! 4

                return ()
            | isCmd args 3 "JOIN" = do
                meta <- asks getMeta
                let nick = args !! 0
                    dest = msg
                    serverurl = getServer meta

                varPath <- asks (variablePathC . getConfig)
                svars <- liftIO $ lines <$> readFile varPath
                let mvar :: [Variable]
                    mvar = do
                        v <- svars
                        let mvars = maybeRead v
                        guard $ mvars /= Nothing
                        let var = fromJust mvars
                        guard $ readableReminder serverurl dest nick var
                        return var
                case length mvar of
                  0 -> return ()
                  1 -> write h $ unwords [ "PRIVMSG"
                                         , dest
                                         , ':' : nick ++ ":"
                                         , varContents $ head mvar
                                         ]
                  _ ->
                    write h $ unwords [ "PRIVMSG"
                                      , dest
                                      , ':' : nick ++ ":"
                                      , "You have " ++ show (length mvar)
                                      , "reminders:"
                                      , unwords $ map varName mvar
                                      ]
            | isCmd args 3 "QUIT" = do 
                let nick = args !! 0

                return ()
            | isCmd args 3 "NICK" = do 
                let nick = args !! 0

                return ()
            | isCmd args 3 "MODE" = do 
                let nick = args !! 0
                    dest = args !! 4
                    mode = args !! 5

                return ()
            | isCmd args 1 "353" = do -- Receive nick list
                let dest = args !! 4 -- use this stuff to handle userlists later
                    --f = \_ -> map (\x'@(x:xs) -> if x `elem` ['~', '&', '@', '%', '+'] then (x, xs) else (' ', x')) $ split " " msg
                    --meta' = f `modUserlist` meta -- meta doesn't hold userlists anymore

                return ()
            | otherwise = do -- Fallback
                return ()
        coreArgs = ["getservers", "getnick", "serverjoin", "serverquit"]
        isCmd args n x | length args >= 3 = args !! 3 == x
                       | otherwise = False

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
