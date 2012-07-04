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
    Finish all the functions
        - Translate
            - New Microsoft App ID fixes this.
        - Dictionary
        - Variable
            - Needs to be rewritten.
        - Calc
            - Needs to be rewritten.
        - Sed
            - Needs to be rewritten.
        - Alias
    Add more entries to help
    Make msgInterpret return [String] instead of String?
    Replace anonymous functions with named functions for readability?
    Add command line arguments, such as making it not autojoin any servers/channels and instead only join ones specified by a certain parameter.
    Clean up Utils.hs
    Core data needs to be parsed, such as `getservers'.
    Make reusable functions instead of bloating up the code everywhere you バカ外人！
    Make Core use the Memory reader Monad?
    Function and channel blacklist

IMPORTANT:
    Make it more memory efficient
        Replace Strings with Text
    Add more lewdness
        Random adjecatives
        50% chance of printing a lewd line every hour (or longer if too frequent)
          This needs a maintained user list or something.
          Alternatively, pull a random username from the logs, unless the logs
          are disabled, in which we either print an empty nick or somehow obtain one.
        Change the lewd file's syntax and add the adjecatives there.
-}

module Main where


import Bot
import Config
import IRC
import Types
import Utils

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Maybe (fromJust, listToMaybe)
import System.Environment (getArgs)
import System.IO
import System.Process


printVersion :: IO ()
printVersion = do
    putStrLn botversion

-- Opens the config file for reading and passes the data to concurrent
-- serverConnect methods.
main :: IO ()
main = do
    args <- getArgs
    if length args > 0
        then checkArg args
        else run serverConnect
  where checkArg x  | head x == "--version" = printVersion
                    | head x == "--test" = run mainTest
                    | otherwise = run serverConnect
        run f = runReaderT f metaConfig
        metaConfig = MetaConfig { getMeta = emptyMeta
                                , getConfig = config
                                }

mainTest :: Memory ()
mainTest = forever $ do
    line <- liftIO getLine
    let msg = "localhost:Owner!Owner@control PRIVMSG #KawaiiBot :" ++ line
    parse stdout msg
  where meta = Meta dest nick name host chans server ownnick
        dest = "#KawaiiBot"
        nick = "Owner"
        name = "Owner"
        host = "localhost"
        userlist = [('~', "Owner"), ('&', "KawaiiBot")]
        chans = ["#KawaiiBot"]
        server = "localhost"
        ownnick = "KawaiiBot"
