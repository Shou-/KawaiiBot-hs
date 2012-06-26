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
import Config
import Utils

import Control.Concurrent
import Control.Exception
import Control.Monad hiding (join)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromJust, listToMaybe)
import Data.String.Utils (split, join, strip)
import Network
import System.IO


-- Writes a String to a handle and prints the message
write :: Handle -> Meta -> String -> IO ()
write h meta str = do
    let message = getServer meta ++ ":" ++ str
    e <- try (hPutStrLn h message) :: IO (Either SomeException ())
    case e of
        Right _ -> putStrLn $ '>' : message
        Left e -> print e

listenLoop :: Handle -> Meta -> IO ()
listenLoop h meta = do
    s <- hGetLine h
    parse h meta s
    listenLoop h meta

parse :: Handle -> Meta -> String -> IO ()
parse h meta bs = do
    let nick = getUsernick meta
        sFull :: [String]
        sFull = ":" `split` bs
        sMsg  = ":" `join` drop 2 sFull
        sArgs = map strip . splits "!@ " . concat $ take 1 $ drop 1 sFull
        meta' = sFull !! 0 `injectServ` meta
    putStrLn $ show sArgs ++ ' ' : sMsg
    if sFull !! 0 `elem` coreArgs
        then return () -- do something you loser
        else parseMsg h meta' (sArgs, sMsg)
  where parseMsg :: Handle -> Meta -> ([String], String) -> IO ()
        parseMsg h meta (args, msg)
            | args !! 3 == "PRIVMSG" = do -- Interpret message and respond if anything's returned from `msgInterpret'
                let nick = args !! 0
                    name = args !! 1
                    host = args !! 2
                    act = args !! 3
                    dest = args !! 4
                    channels = getChannels meta
                    serverurl = getServer meta
                    ownnick = getOwnNick meta

                    meta' = Meta dest nick name host channels serverurl ownnick

                when urlFetching . void . forkIO $ do -- URL fetching
                    let urls = filter (\x -> if take 4 x `elem` ["http"] then True else False) $ words msg
                    forM_ urls $ \url -> title url >>= \title' -> unless (null title') . write h meta' $ "PRIVMSG " ++ dest ++ " :\ETX5Title\ETX: " ++ title'

                post <- msgInterpret meta' msg
                let mAct = if isChannelMsg post
                        then "PRIVMSG " ++ dest
                        else "PRIVMSG " ++ nick
                    msg' = fromMsg post

                unless (null msg') . write h meta' $ mAct ++ " :" ++ msg'

                when msgLogging $ do -- logging
                    e <- try (do
                        let path = logPath ++ serverurl ++ " " ++ dest
                        appendFile path $ show (args, msg) ++ "\n") :: IO (Either SomeException ())
                    case e of
                        Right _ -> return ()
                        Left e -> when debug $ print e

                return ()
            | args !! 3 == "INVITE" = do -- Join a channel on invite
                let nick = args !! 0
                    name = args !! 1
                    host = args !! 2
                    act = args !! 3
                    dest = args !! 4
                    channels = getChannels meta
                    serverurl = getServer meta
                    ownnick = getOwnNick meta

                    meta' = Meta dest nick name host channels serverurl ownnick

                -- print reminder message if any

                -- check this channel against a list of banned ones or something
                write h meta' $ "JOIN " ++ msg
                return ()
            | args !! 3 == "KICK" = do -- Single nicklist update
                let dest = args !! 4

                return ()
            | args !! 3 == "PART" = do -- Single nicklist update
                let nick = args !! 0
                    dest = args !! 4

                return ()
            | args !! 3 == "JOIN" = do
                let nick = args !! 0
                    dest = msg

                -- get :reminder variable
                return ()
            | args !! 3 == "QUIT" = do -- Full nicklist update
                let nick = args !! 0
                    f = foldr (\(x, nick') acc -> if nick' == nick then acc else (x, nick') : acc) []

                return ()
            | args !! 3 == "NICK" = do -- Full nicklist update
                let nick = args !! 0
                    f = foldr (\(x, nick') acc -> if nick' == nick then (x, msg) : acc else (x, nick') : acc) []

                return ()
            | args !! 3 == "MODE" = do -- Full nicklist update; Figure out a way to handle this. Make new `data' for modes?
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
        coreArgs = ["getservers", "getnick", "getchannels"]

-- Connects to an IRC server
serverConnect :: IO ()
serverConnect = do
    h <- connectTo "localhost" (PortNumber $ fromIntegral 3737)
    hSetEncoding h utf8
    hSetBuffering h LineBuffering
    hSetNewlineMode h (NewlineMode CRLF CRLF)
    forkIO . forever $ getLine >>= hPutStrLn h
    let meta = Meta [] [] [] [] [] [] []
    listenLoop h meta
