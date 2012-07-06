
{-

Make a copy of this file and rename it to "Config.hs" and put it in the same
directory as the rest of the source files, then modify it to your liking.

If there is an error in this example, please report it through Github!
https://github.com/Shou-/KawaiiBot-hs

-}

module Config (config) where

-- this imports the Types needed
import Types
import Utils
-- this imports event functions.
import Bot

-- this directory must exist
storagePath = "/home/username/irc/"

{- Available event functions are:
    - anime
    - random
    - sed
    - airing
    - help
    - lastMsg
    - lewd
    - manga
    - random
    - title
    - weather
    - wiki

See README.md for information about what arguments they take. Please note that 
`optional' does not apply to the arguments these functions take. They must be 
provided with everything listed, but `empty' is just an empty list, so if you 
do not want to pass anything to it, just use `[]' (empty list).

Examples:

Prints the defaults when using the anime function.
    eventFunc = anime [] []

Prints 10 results received from searching for YuruYuri.
    eventFunc = anime "YuruYuri" ["10"]

It just prints a lewd string. Remember, lewd takes no arguments.
    eventFunc = lewd

This one pipes the weather into sed. It might not be so easy to understand to
none Haskellers.
    eventFunc = weather "Oslo, Norway" >>= (sed "/Today/Right now/ " ++)

We can write it in another way
    eventFunc = do
        we <- weather "Oslo, Norway"
        sed "/Today/Right now/ " ++ we
-}

                            -- event functions
                            -- print a random message from the log for this channel
randomMsg = defaultEvent { eventFunc = random "1000" >>= lastMsg

                            -- how frequently it will run (every hour here)
                         , eventRunTime = return 3600

                            -- chance of executing the event
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
                                    -- NickServ password
                  , serverNSPass = ""
                                    -- The only channels KawaiiBot should join.
                  , allowedChannels = Whitelist ["#kawaiibot"]
                  , allowedFuncs = []
                  }
rizon = Server { serverURL = "irc.rizon.net"
                  , serverChans = ["#KawaiiBot"]
                  , serverPort = 6667
                  , serverNick = "QuteBot"
                  , serverNSPass = ""
                                    -- Channels that KawaiiBot shouldn't join.
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
                  , titleFetchingC  = True

                    -- whether to log messages or not
                  , msgLoggingC     = True

                    -- level of verbosity
                        -- 0 prints no errors
                        -- 1 prints important errors
                        -- 2 prints general information and important errors
                  , verbosityC      = 1
                  }
