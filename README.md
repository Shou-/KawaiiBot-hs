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
msAppId = "key"
```

To install, run `cabal install`.
To run, execute `kawaiibot-core` and `kawaiibot-client`.

## Functions
* `.help`
This function sends the general help string to the user who typed this.
It takes one argument, the name of a function.

* `.>`
Prints a message to the channel, or user if it is a private message.
It takes a string as an argument.

* `.<`
Prints a message to the user.
It acts the same way as `.>`.

* `.sed`
A regex replace function.
It takes two arguments, the regex matching and replacing string (ex: `/banana/apple/`) and a string.

* `.ai`
Prints currently airing anime.
It takes one colon argument, an integer. (ex: `.ai:10`, will print 10 of the fetched airing anime)

* `.ra`
Prints a random number or string.
Takes one integer as an argument, or several strings separated by `|` (pipe).

* `.we`
Prints weather information for the specified location.
Takes one string argument.

* `.lewd`
Prints a random string from the file as defined in `Config.hs`.
Takes zero arguments.

## Operators
* `>>`
Bind, it executes the function on the left but ignores the output, and continues parsing the right.

* `->`
Pipe, it appends the output of the function on the left into the function on the right.

* `++`
Add, it appends the string of the output on the right to the output on the left.

## .lewd
The format for the lewd function's file is currently looking something like this:
```
"Hey! This is a string! Woohoo~ \ETX04\x2665\ETX"
"It's another string! They are separated by newlines."
```
Where `\ETX` is the character needed to color text over IRC and `\x2665` is the unicode 'black heart' character.
