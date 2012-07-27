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

{-
TODO:
    - Finish all the functions
        - Translate
            - New Microsoft App ID fixes this.
        - Dictionary
        - Variable
            - Global variables
            - List
        - Calc
            - Needs to be rewritten.
        - Help
            - Make it print whether the function/operator is black/whitelisted
              or not.
        - AniDB
        - Lambdas ?
            - .\ x y => .an:x y
            - Can be stored in .$ variables
            - Can be used with planned parens
        - Diff
            f x y = filter (not . (`elem` y)) x
    - Add command line arguments, such as making it not autojoin any
      servers/channels and instead only join ones specified by a certain
      parameter.
    - Core data needs to be parsed, such as `getservers'.
    - Add `owner' to Server data.
        - Be able to control KawaiiBot through ownership
            - Quit servers/channels
            - Join Servers/channels
            - Other stuff
    - Add operator escaping or make certain functions ignore operators.
    - Add parens for the functions/operators
        - `.lewd -> (.ra 100 -> .sed s/matcher/replacer/)`
    - Add timeouts between usage of functions that can becustomized per channel
      in Config.hs.
        - Per channel and user or just channel timeout?
    - Let `events' store data for later use.
        - Make event functions loop in their own threads and pass data to each
          other. This will allow functions such as `diff' to work properly.
            - Even functions that output the same to all channels.
            - Odd functions that run the event function for each channel.
    - Add more lewdness
        - Random adjecatives, verbs and body parts.
        - Change the lewd file's syntax.
    - Remove unnecessary modules.
        - mtl
-}

module Main where


import Config

import KawaiiBot.Bot
import KawaiiBot.IRC
import KawaiiBot.Types
import KawaiiBot.Utils

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
  where meta = Meta dest nick name host chans server temp
        dest = "#KawaiiBot"
        nick = "Owner"
        name = "Owner"
        host = "localhost"
        userlist = [('~', "Owner"), ('&', "KawaiiBot")]
        chans = ["#KawaiiBot"]
        server = "localhost"
        temp = []
