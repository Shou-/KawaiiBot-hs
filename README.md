KawaiiBot Haskell
=================

This is a Haskell rewrite of KawaiiBot.
KawaiiBot is a lewd IRC bot that does useless things, such as checking the weather, grabbing information about anime and manga, and printing lewd messages.

See `Config.hs.example` for options.

To install, run `cabal install`.
To run KawaiiBot, execute `kawaiibot-core` and `kawaiibot-client`.

## Functions

All functions need to be prefixed with '.' or '!'.

* `.help`
This function sends the general help string to the user who typed this.<br>
It takes one optional argument, the name of a function.<br>
Example: `.help ai`

* `.>`
Prints a message to the channel, or user if it is a private message.<br>
It takes a string as an argument.<br>
Example: `.> I-it's not like I'm sending this because I want to or a-anything, idiot!`

* `.<`
Prints a message to the user.<br>
It acts the same way as `.>`.<br>
Example: `.< Psst, KawaiiBot! please print some lewd messages.`

* `.^`
Gets the last message from the channel's log.<br>
It takes one argument, an integer, starting at zero.<br>
Example: `.^ 10`

* `.sed`
A regex replace function.<br>
It takes two arguments, the regex matching and replacing string and a string.<br>
Example: `.> I love bananas! -> .sed /banana/apple/`

* `.ai`
Prints currently airing anime.<br>
It takes one optional colon argument, an integer and amount of anime to print.<br>
Example: `.ai:5`

* `.an`
Prints recent anime releases.<br>
It takes two optional arguments. The number of results to print and the anime to search for.<br>
Example: `.an:5 yuruyuri -HorribleSubs`

* `.ma`
Prints recent manga releases.<br>
It takes two optional arguments. The number of results to print and the manga to search for.<br>
Example: `.ma:2 banana no nana`

* `.ra`
Prints a random number or string.<br>
Takes one integer as an argument, or several strings separated by `|` (pipe).<br>
Example: `.ra Suwako|Momiji|Youmu`

* `.we`
Prints weather information for the specified location.<br>
Takes one string argument, either a location or a postal code.<br>
Example: `.we Oslo, Norway`

* `.lewd`
Prints a random string from the file as defined in `Config.hs`.<br>
Takes zero arguments.<br>
Example: `.lewd ++ .lewd ++ .lewd ++ .lewd`

* `.wiki`
Prints the top paragraph of a Wikipedia article.<br>
Takes one argument, the article title.<br>
Example: `.wiki haskell programming language`

* `.isup`
Prints the status of a website.<br>
Takes one argument, an URL.<br>
Example: `.isup haskell.org`

## Operators
* `>>`
Bind, it executes the function on the left but ignores the output, and continues parsing the right.<br>

* `->`
Pipe, it appends the output of the function on the left into the function on the right.

* `++`
Add, it appends the string of the output on the right to the output on the left.

Example usage: `.ra 100 -> .^ ++ .lewd`

## .lewd
The format for the lewd function's file is currently looking something like this:
```
"Hey! This is a string! Woohoo~ \ETX04\x2665\ETX"
"It's another string! They are separated by newlines."
"Also, %(user) is a dummy-face!"
```
Where `\ETX` is the character needed to color text over IRC, `\x2665` is the unicode 'black heart' character and %(user) will be replaced by the nick of the one using the `.lewd` function.
