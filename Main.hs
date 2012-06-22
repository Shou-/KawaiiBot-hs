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

{- Some of this is outdated since the Core rewrite
TODO:
    Make MODEs work properly.
        Alternatively, strip functions that require MODEs entirely.
    Finish all the functions
        Finish the translate function
        Finish the wikipedia function
        Fix the anime and airing functions
        Add a dictionary lookup function
    Add more entries to help
    Function aliases
    Ping timeout; if a PING hasn't been received within a certain timeframe, try reconnecting.
    Fix all ``fix this'' commented blocks
    Make msgInterpret return [String] instead of String? -- Strings are replaced by ByteStrings
    Replace anonymous functions with named functions
    Upload it to Github.
        Replace old, move old source to a directory and eventually remove it completely when KawaiiBot Haskell is good enough.
    Add command line arguments, such as making it not autojoin any servers/channels and instead only join ones specified by a certain parameter.

FIXME:
    Rewrite the calc function

IMPORTANT:
    Make it more memory efficient
        Replace Strings with ByteStrings
        Replace lists with DiffLists
    Add more lewdness
        Random adjecatives
        50% chance of printing a lewd line every hour (or longer if too frequent)
-}

module Main where

{-# LANGUAGE DoAndIfThenElse, MultiParamTypeClasses, TypeSynonymInstances,
    FlexibleInstances #-}

import Bot
import Config
import IRC
import Utils

import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.Maybe (fromJust, listToMaybe)
import System.Environment (getArgs)
import System.Process

printHelp :: IO ()
printHelp = do
    putStrLn "KawaiiBot version 2012.06.19 16:29"

-- Opens the config file for reading and passes the data to concurrent
-- serverConnect methods.
main :: IO ()
main = do
    args <- getArgs
    if length args > 0
        then checkArg args
        else serverConnect
  where checkArg x  | head x == "--help" = printHelp
                    | head x == "--test" = mainTest
                    | otherwise          = serverConnect

mainTest :: IO ()
mainTest = forever $ getLine >>= msgInterpret meta >>= putStrLn . snd
  where meta = Meta dest nick name host chans server ownnick
        dest = "#KawaiiBot"
        nick = "Shou-"
        name = "Shou"
        host = "Tora.maru"
        userlist = [('~', "Shou-"), ('&', "KawaiiBot")]
        chans = ["#KawaiiBot"]
        server = "localhost"
        ownnick = "KawaiiBot"
