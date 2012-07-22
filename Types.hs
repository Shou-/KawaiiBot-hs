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

module Types where


import Control.Concurrent (MVar)
import Control.Monad.Reader
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

-- server, server, nick, name, contents
data Variable = Immutable String String String String String
              | Reminder String String String String String
              | Personal String String String String String
              | Normal String String String String String
              | Global String String String
              deriving (Show, Read, Eq)

data Message a = ChannelMsg a
               | UserMsg a
               | EmptyMsg
               deriving (Eq, Show)

instance Functor Message where
    fmap f (ChannelMsg x) = ChannelMsg (f x)
    fmap f (UserMsg x) = UserMsg (f x)
    fmap f EmptyMsg = EmptyMsg

-- For interpreting IRC messages
data Funa = Add String Funa
          | Pipe String Funa
          | Bind String Funa
          | App String Funa
          | Plain String
          | Void
          deriving (Show, Read)

-- For Config
data Allowed a = Blacklist { getBlacklist :: a }
               | Whitelist { getWhitelist :: a }
               deriving (Show)

-- Configuration data for KawaiiBot
data Config = Config { serversC :: [Server]
                     , eventsC :: [Event]
                     , lewdPathC :: FilePath
                     , logsPathC :: FilePath
                     , variablePathC :: FilePath
                     , msAppIdC :: String
                     , msgLoggingC :: Bool
                     , verbosityC :: Int
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

-- Define a server for Config
data Server = Server { serverPort :: Int
                     , serverURL :: String
                     , serverChans :: [String]
                     , serverNick :: String
                     , serverNSPass :: String
                     , allowedChannels :: Allowed [String]
                     , allowedFuncs :: [(String, Funcs)]
                     } deriving (Show)

defaultServer = Server { serverPort = 6667
                       , serverURL = ""
                       , serverChans = []
                       , serverNick = ""
                       , serverNSPass = ""
                       , allowedChannels = Blacklist []
                       , allowedFuncs = []
                       }

-- A timed event for Config
data Event = Event { eventFunc :: Memory String
                   , eventRunTime :: IO Double
                   , eventTime :: Double
                   , eventChance :: Double
                   , eventServers :: [Server]
                   , eventTemp :: [String]
                   }

defaultEvent = Event { eventFunc = return ""
                     , eventRunTime = return 0
                     , eventTime = 0
                     , eventChance = 0
                     , eventServers = []
                     , eventTemp = []
                     }

-- Message metadata
data MsgFunc = MsgFunc { msgFunc :: String
                       , msgArgs :: [String]
                       , msgString :: String
                       }

data Meta = Meta { getDestino :: String
                 , getUsernick :: String
                 , getUsername :: String
                 , getHostname :: String
                 , getChannels :: [String]
                 , getServer :: String
                 , getTemp :: [String]
                 } deriving (Show, Eq)

emptyMeta = Meta [] [] [] [] [] [] []

data Funcs = Funcs { allowEcho :: Bool
                   , allowTitle :: Bool
                   , allowWeather :: Bool
                   , allowAnime :: Bool
                   , allowAiring :: Bool
                   , allowManga :: Bool
                   , allowWiki :: Bool
                   , allowIsup :: Bool
                   , allowSed :: Bool
                   , allowLewd :: Bool
                   , allowRandom :: Bool
                   , allowHistory :: Bool
                   , allowVariable :: Bool
                   , allowTranslate :: Bool
                   , allowBind :: Bool
                   , allowPipe :: Bool
                   , allowAdd :: Bool
                   , allowApp :: Bool
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


-- Client
type Memory = ReaderT MetaConfig IO
data MetaConfig = MetaConfig { getMeta :: Meta, getConfig :: Config }

-- Core
type CMemory = ReaderT MVarConfig IO
data MVarConfig = MVarConfig { getcChannels :: [(String, [String])]
                             , getcMVars :: MVars
                             , getcConfig :: Config
                             }

data MVars = MVars { clientsMVar :: MVar [CClient]
                   , textMVar :: MVar [CMessage]
                   , serverMVar :: MVar [CServer]
                   , timeMVar :: MVar [CTimestamp]
                   }

type CClient = (String, Handle)
type CServer = (String, Handle)
type CMessage = (String, T.Text)
type CTimestamp = (String, Int)
