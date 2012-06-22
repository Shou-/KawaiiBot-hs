KawaiiBot Haskell
=================

This is a Haskell rewrite of KawaiiBot.
KawaiiBot is a lewd IRC bot that does useless things, such as checking the weather, grabbing information about anime and manga, and printing lewd messages.

To use it, you need to create `Config.hs` and add some Haskell variables to it:
```
-- This file just needs to exist for the .$ function to work.
variablePath = "/filepath/filename"
-- The file containing strings printed by the `.lewd' function.
-- Phrases need to be added manually; see below.
lewdPath = "/filepath/filename"
-- This is to be able to use Bing Translate.
msAppId
```

To install, run `cabal install`.
To run, execute `kawaiibot-core` and `kawaiibot-client`.

## .lewd
The format for the lewd function is currently looking something like this:
```
"Hey! This is a string! Woohoo~ \ETX04\x2665\ETX"
```
Where \ETX is the character needed to color text over IRC and \x2665 is the unicode 'black heart' character.
