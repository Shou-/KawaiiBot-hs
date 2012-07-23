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

{-# LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module KawaiiBot.Types where


import Control.Applicative
import Control.Concurrent (MVar)
import Control.Monad.Reader
import Data.Monoid
import Data.String.Utils (replace)
import qualified Data.Text as T
import System.IO (Handle)


class PyFormat s r where
    (%) :: s -> r -> s

instance PyFormat String [(String, String)] where
    (%) x y = foldr (\(s1, s2) x' -> replace ("%(" ++ s1 ++ ")") s2 x') x y

instance PyFormat String (String, String) where
    (%) x (s1, s2) = replace ("%(" ++ s1 ++ ")") s2 x

instance PyFormat String String where
    (%) x y = replace "%s" y x

-- | Data for the variable function in "KawaiiBot.Bot".
data Variable = Immutable String String String String String
                -- ^ IRC server, channel, nick, var name and var contents.
              | Reminder String String String String String
                -- ^ IRC server, channel, nick, var name and var contents.
              | Personal String String String String String
                -- ^ IRC server, channel, nick, var name and var contents.
              | Normal String String String String String
                -- ^ IRC server, channel, nick, var name and var contents.
              | Global String String String
                -- ^ Nick, var name and var contents.
              deriving (Show, Read, Eq)

-- | Message data for functions 
data Message a = ChannelMsg a
                -- ^ Message that goes to the channel, or user if none.
               | UserMsg a
                -- ^ Message that goes to the user.
               | EmptyMsg
               deriving (Eq, Show)

instance Functor Message where
    fmap f (ChannelMsg x) = ChannelMsg (f x)
    fmap f (UserMsg x) = UserMsg (f x)
    fmap f EmptyMsg = EmptyMsg

-- | For interpreting IRC messages
data Funa = Add String Funa
            -- ^ The \`++' IRC operator
          | Pipe String Funa
            -- ^ The \`->' IRC operator
          | Bind String Funa
            -- ^ The \`>>' IRC operator
          | App String Funa
            -- ^ The \`$$' IRC operator
          | Plain String
            -- ^ A plain function where no operator follows
          | Void
            -- ^ Empty, only used if the IRC message parser reaches an error
          deriving (Show, Read)

-- | Blacklist or whitelist a list of something, for example IRC channels.
-- Used in Config.hs
data Allowed a = Blacklist { getBlacklist :: a }
               | Whitelist { getWhitelist :: a }
               deriving (Show)

-- | Configuration data for KawaiiBot
data Config = Config { serversC :: [Server]
                    -- ^ IRC servers to join
                     , eventsC :: [Event]
                    -- ^ Bot events
                     , lewdPathC :: FilePath
                    -- ^ Path of the file read by `"KawaiiBot.Bot.lewd'.
                     , logsPathC :: FilePath
                    -- ^ Directory where log files are stored.
                     , variablePathC :: FilePath
                    -- ^ Path of the file used by `KawaiiBot.Bot.variable2'.
                     , msAppIdC :: String
                    -- ^ Microsoft app ID used by `KawaiiBot.Bot.translate'.
                     , msgLoggingC :: Bool
                    -- ^ Whether to log or not.
                     , verbosityC :: Int
                    -- ^ Verbosity level.
                    -- 0: No messages. 1: Errors. 2: Debug and errors.
                     }

defaultConfig = Config { serversC = []
                       , eventsC = []
                       , lewdPathC = ""
                       , logsPathC = ""
                       , variablePathC = ""
                       , msAppIdC = ""
                       , msgLoggingC = False
                       , verbosityC = 1
                       }

-- | Define a server for Config
data Server = Server { serverPort :: Int
                    -- ^ Server port; usually 6667.
                     , serverURL :: String
                    -- ^ IRC server URL.
                     , serverChans :: [String]
                    -- ^ IRC server channels.
                     , serverNick :: String
                    -- ^ Nickname of the bot.
                     , serverNSPass :: String
                    -- ^ Nickserv password for registration.
                     , allowedChannels :: Allowed [String]
                    -- ^ Which channels KawaiiBot can be invited to.
                     , allowedFuncs :: [(String, Funcs)]
                    -- ^ Which functions are allowed to be used.
                    -- Takes a list of channel and Funcs.
                     } deriving (Show)

defaultServer = Server { serverPort = 6667
                       , serverURL = ""
                       , serverChans = []
                       , serverNick = ""
                       , serverNSPass = ""
                       , allowedChannels = Blacklist []
                       , allowedFuncs = []
                       }

-- | A timed event for Config
-- Events will most likely become saner in the future.
data Event = Event { eventFunc :: Memory String
                    -- ^ The function that outputs a string
                    -- which is then printed to the channel.
                   , eventRunTime :: IO Double
                    -- ^ How often to run the event in seconds.
                   , eventTime :: Double
                    -- ^ Used by `KawaiiBot.IRC.event' to store a timestamp.
                   , eventChance :: Double
                    -- ^ Chance of success. From 0.0 to 1.0.
                   , eventServers :: [Server]
                    -- ^ Which servers/channels to run the event on.
                   , eventTemp :: [String]
                    -- ^ Temporary data stored by event functions.
                   }

defaultEvent = Event { eventFunc = return ""
                     , eventRunTime = return 0
                     , eventTime = 0
                     , eventChance = 0
                     , eventServers = []
                     , eventTemp = []
                     }

-- | Message metadata.
-- What the hell is this?
data MsgFunc = MsgFunc { msgFunc :: String
                       , msgArgs :: [String]
                       , msgString :: String
                       }

-- | Data read by 
data Meta = Meta { getDestino :: String
                -- ^ Destination; a channel or nick.
                 , getUsernick :: String
                -- ^ Nick of the user who sent the message.
                 , getUsername :: String
                -- ^ Username of the user who sent the message.
                 , getHostname :: String
                -- ^ Hostname of the user who sent the message.
                 , getChannels :: [String]
                -- ^ This isn't even used!
                 , getServer :: String
                -- ^ Server the message came from.
                 , getTemp :: [String]
                -- ^ wat
                 } deriving (Show, Eq)

emptyMeta = Meta [] [] [] [] [] [] []

-- | Data to define which functions are allowed and which aren't.
data Funcs = Funcs { allowEcho :: Bool
                    -- ^ Allow usage of the printing functions, \`.>' and \`.<'.
                   , allowTitle :: Bool
                    -- ^ Allow KawaiiBot to print titles of websites.
                   , allowWeather :: Bool
                    -- ^ Allow usage of the weather function, `.we'.
                   , allowAnime :: Bool
                    -- ^ Allow usage of the anime release function, `.an'.
                   , allowAiring :: Bool
                    -- ^ Allow usage of the anime airing function, `.ai'.
                   , allowManga :: Bool
                    -- ^ Allow usage of the manga release function, `.ma'.
                   , allowWiki :: Bool
                    -- ^ Allow usage of the Wikipedia function, `.wiki'.
                   , allowIsup :: Bool
                    -- ^ Allow usage of the website `is up' function, `.isup'.
                   , allowSed :: Bool
                    -- ^ Allow usage of the regex replace function, `.sed'
                   , allowLewd :: Bool
                    -- ^ Allow usage of the random string function, `.lewd'.
                   , allowRandom :: Bool
                    -- ^ Allow usage of the random numer/choice function, `.ra'.
                   , allowHistory :: Bool
                    -- ^ Allow usage of the history fetching function, \`.^'.
                   , allowVariable :: Bool
                    -- ^ Allow usage of the variable storing function, \`.$'.
                   , allowTranslate :: Bool
                    -- ^ Allow usage of the translation function, `.tr'.
                    -- It doesn't ``just werk''!
                   , allowBind :: Bool
                    -- ^ Allow usage of the bind operator, \`>>'.
                   , allowPipe :: Bool
                    -- ^ Allow usage of the pipe operator, \`->'.
                   , allowAdd :: Bool
                    -- ^ Allow usage of the add operator \`++'.
                   , allowApp :: Bool
                    -- ^ Allow usage of the reverse pipe operator \`$$'.
                   } deriving (Show)

defaultFuncs = Funcs { allowEcho = True
                     , allowTitle = False
                     , allowWeather = True
                     , allowAnime = True
                     , allowAiring = True
                     , allowManga = True
                     , allowWiki = True
                     , allowIsup = True
                     , allowSed = True
                     , allowLewd = True
                     , allowRandom = True
                     , allowHistory = True
                     , allowVariable = True
                     , allowTranslate = False
                        -- Operators
                     , allowBind = False
                     , allowPipe = True
                     , allowAdd = True
                     , allowApp = True
                     }

-- | Reader monad used by Client.hs and its sub-modules.
type Memory = ReaderT MetaConfig IO
-- | Data to store in the Reader monad.
data MetaConfig = MetaConfig { getMeta :: Meta, getConfig :: Config }

-- ** Core
-- | Reader monad used by Core.hs and its sub-modules.
type CMemory = ReaderT MVarConfig IO
-- | Data to store in the Reader monad.
data MVarConfig = MVarConfig { getcChannels :: [(String, [String])]
                             , getcMVars :: MVars
                             , getcConfig :: Config
                             }

-- | MVars used by Core.hs
data MVars = MVars { clientsMVar :: MVar [CClient]
                   , textMVar :: MVar [CMessage]
                   , serverMVar :: MVar [CServer]
                   , timeMVar :: MVar [CTimestamp]
                   }

-- | Core client data, client hostname and handle.
type CClient = (String, Handle)
-- | Core server data, IRC server URL and handle.
type CServer = (String, Handle)
-- | Core message data, IRC server URL and text.
type CMessage = (String, T.Text)
-- | Core timestamp data, IRC server URL and timestamp.
type CTimestamp = (String, Int)
