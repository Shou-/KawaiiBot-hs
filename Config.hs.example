
{-

Make a copy of this file and rename it to "Config.hs" and put it in the same
directory as the rest of the source files, then modify it to your liking.

-}

module Config (config) where

-- this imports the Types needed
import Types
import Utils
-- this imports event functions.
import Bot


storagePath = "/home/username/irc/"

                            -- event functions
                            -- print a random message from the log for this channel
randomMsg = defaultEvent { eventFunc = random "1000" >>= lastMsg

                            -- how frequently it will run (every hour here)
                         , eventRunTime = return 3600

                            -- chance of success
                         , eventChance = 0.7

                            -- servers/channels it prints to
                         , eventServers = [eventRizon]
                         }
                            -- print airing anime
animeAiring = defaultEvent { eventFunc = airing ["100"]
                           , eventRunTime = return 3600
                           , eventChance = 1
                           , eventServers = [eventRizon]
                           }

                            -- server for events
                            -- only server and channels are necessary
eventRizon = defaultServer { serverURL = "irc.rizon.net"
                           , serverChans = ["#KawaiiBot"]
                           }

freenode = Server { serverURL = "irc.freenode.net"
                  , serverChans = ["#mychannel"]
                  , serverPort = 6667
                  , serverNick = "QuteBot"
                  , serverNSPass = ""
                  , allowedChannels = Blacklist []
                  , allowedFuncs = []
                  }
rizon = Server { serverURL = "irc.rizon.net"
                  , serverChans = ["#KawaiiBot"]
                  , serverPort = 6667
                  , serverNick = "KawaiiBot"
                  , serverNSPass = ""
                  , allowedChannels = Blacklist ["#malicious", "#channels"]
                  , allowedFuncs = []
                  }

config =            -- file where variables are stored by the `.$' function.
    defaultConfig { variablePathC   = storagePath ++ "variables"

                    -- file where lines printed by `.lewd' are read.
                  , lewdPathC       = storagePath ++ "lewds"

                    -- directory where logs are stored.
                  , logsPathC       = storagePath ++ "logs/"

                  , msAppIdC        = ""

                    -- events
                  , eventsC         = [randomMsg, animeAiring]

                    -- servers
                  , serversC        = [freenode, rizon]

                    -- whether to print URL titles or not
                  , urlFetchingC    = True

                    -- whether to log messages or not
                  , msgLoggingC     = True

                    -- level of verbosity
                        -- 0 prints no errors
                        -- 1 prints important errors
                        -- 2 prints general information and important errors
                  , verbosityC      = 2
                  }
