
{-

Make a copy of this file and rename it to "Config.hs" and put it in the same
directory as the rest of the source files, then modify it to your liking.

If there is an error in this example, please report it through Github!
https://github.com/Shou-/KawaiiBot-hs

-}

module KawaiiBot.Config (config) where

-- this imports the Types needed
import KawaiiBot.Types
import KawaiiBot.Utils
-- this imports event functions.
import KawaiiBot.Bot

-- this directory must exist
storagePath = "/home/username/irc/"

{- Available event functions are:
    - channelMsg
    - userMsg
    - anime
    - random
    - sed
    - airing
    - help
    - findMsg
    - lewd
    - manga
    - random
    - title
    - weather
    - wiki
    - isup

All functions take two arguments, one list of strings (the colon arguments) and
a string. See `README.md' for information about the functions and what the
strings should contain.

Examples:

Prints anime releases matching `yuru yuri':
    eventFunc = plain (anime [] "yuru yuri")

Prints 2 anime releases matching `yuru yuri' added to a custom string:
    eventFunc = add (channelMsg "These anime are fun:") (anime ["2"] "yuru yuri")
Altternative:
    eventFunc = return "These anime are fun:" `add` anime ["2"] "yuru yuri"

Prints 20 anime recent anime releases and pipes it into the regex replace
function that removes brackets and their contents:
    eventFunc = pipe (anime ["20"] "") sed [] "s/\[.+\]//"

Helper functions:
    - plain
        Plain takes one argument
            1. A function
    - pipe
        Pipe takes four arguments
            1. A function
            2. The name of a function
            3. The list of strings (colon arguments)
            4. The string to append 1's output to
    - add
        Add takes two arguments
            1. A function
            2. A function
    - bind
        Add takes two arguments
            1. A function
            2. A function

-}

                            -- event functions
                            -- print a random message from the log for this channel
randomMsg = defaultEvent { eventFunc = pipe (random [] "1000") findMsg [] ""

                            -- how frequently it will run (every hour here)
                         , eventRunTime = return 3600

                            -- chance of executing the event
                         , eventChance = 0.7

                            -- servers/channels it prints to
                         , eventServers = [eventRizon]
                         }
                            -- print airing anime
animeAiring = defaultEvent { eventFunc = plain (airing ["10"] "")
                           , eventRunTime = return 3600
                           , eventChance = 1
                           , eventServers = [eventRizon]
                           }

                            -- server for events
                            -- only server and channels are necessary
eventRizon = defaultServer { serverURL = "irc.rizon.net"
                            -- Note: blacklisting is not yet implemented,
                            --       do not use it.
                           , allowedChannels = Whitelist ["#KawaiiBot"]
                           }

freenode = Server { serverURL = "irc.freenode.net"
                  , serverChans = ["#mychannel"]
                  , serverPort = 6667
                  , serverNick = "QuteBot"
                                    -- NickServ password
                  , serverNSPass = ""
                                    -- The only channels KawaiiBot should join
                                    -- if invited.
                  , allowedChannels = Whitelist ["#kawaiibot"]
                  , allowedFuncs = []
                  }
rizon = Server { serverURL = "irc.rizon.net"
                  , serverChans = ["#KawaiiBot"]
                  , serverPort = 6667
                  , serverNick = "QuteBot"
                  , serverNSPass = ""
                                    -- Channels that KawaiiBot shouldn't join
                                    -- if invited.
                  , allowedChannels = Blacklist ["#malicious", "#channels"]
                  , allowedFuncs = funcs
                  }
                    -- channel where the allowed functions apply to
  where funcs = [ ("#KawaiiBot",
                            -- See `Types.hs' if you want to see what the names
                            -- of the variables are and the default values.
                            -- Search for `defaultFuncs'.

                                        -- Allow title fetching for URLs
                        defaultFuncs { allowTitle = True
                                        -- Disallow printing of lewd messages
                                     , allowLewd = False
                                     }
                  )
                , ("#someotherchannel", defaultFuncs)
                ]

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

                    -- whether to log messages or not
                  , msgLoggingC     = True

                    -- level of verbosity
                        -- 0 prints no errors
                        -- 1 prints important errors
                        -- 2 prints general information and important errors
                  , verbosityC      = 1
                  }
