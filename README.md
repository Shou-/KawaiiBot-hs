KawaiiBot Haskell
=================

This is a Haskell rewrite of KawaiiBot.
KawaiiBot is a lewd IRC bot that does useless things, such as checking the weather, grabbing information about anime and manga, and printing lewd messages.

See `Config.example.hs` for options.

To install, run `cabal install`.
To run KawaiiBot, execute `kawaiibot-core` and `kawaiibot-client`.

## Functions

All functions need to be prefixed with `.` or `!`.

* `.help`
This function sends the general help string to the user who typed this.<br>
It takes one optional argument, the name of a function or an operator.<br>
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
Searches for a message in the bot's chatlog.<br>
It takes two optional arguments. The index of the list of matches, and a search string. If no search string is given, it matches against everything.<br>
Example: `.^:4 banana -apple`

* `.$`
Variable storing/fetching function.<br>
It takes one optional colon argument, the user who should take ownership of the variable, and a string in the variable syntax.<br>
Example: `.$:nick Reminder linux = Please remember to install a distribution of your choice!`<br>
Example: `.$ linux`<br>
Example: `.$ Remove linux = I don't like this anymore`<br>
Where `Reminder` is optional and can be replaced with `Personal`, `Immutable` or `Normal`. The second example just prints the variable contents.<br>
`Personal`: Only printable by the variable owner. `Reminder`: Same as personal, but may be printed when the variable owner joins the channel. `Immutable`: Can only be modified by the owner. `Normal`: Printable and modifiable by all. `Remove`: Deletes a variable.

* `.sed`
A regex replace function.<br>
It takes two arguments, the regex matching and replacing string and a string.<br>
Example: `.> I love bananas! -> .sed s/banana/apple/`

* `.ai`
Prints currently airing anime.<br>
It takes two optional arguments. The number of results to print and the anime to search for.<br>
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
Bind, it executes the function on the left but ignores the output.<br>
Example: `.lewd >> .ai`

* `->`
Pipe, it appends the output of the function on the left into the function on the right.<br>
Example: `.lewd -> .>`

* `$$`
Application operator, it appends the output of the function on the right into the function on the left. The opposite of `->`.<br>
Example: `.we $$ .ra Tokyo|Oslo|Madrid`

* `++`
Add, it appends the string of the output on the right to the output on the left.<br>
Example: `.lewd ++ .lewd`

## .lewd
The format for the lewd function's file is currently looking something like this:
```
lewds:
    %(greet) This is a string! Woohoo~ ♡
    It's another string! They are separated by newlines.
    Also, %(user) is a %(insult)!
greet:
    Hey!
    Hello!
    Hola!
insult:
    dummy-face
    stupid idiot
    ⑨
```
The `lewds` list must be there, however, everything else is a variable that you
can use inside the `lewds`. %(user) is replaced by the nick of the user
executing the `.lewd` function.
