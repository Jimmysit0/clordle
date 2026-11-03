# (clordle)

Wordle, but every answer is a Clojure function!

Play it at **[jimmysit0.github.io/clordle](https://jimmysit0.github.io/clordle/)**

https://github.com/user-attachments/assets/bdb3674b-3ef5-4cf6-871f-496ff1b8958d

You get six tries to guess a four-letter function name, and your guesses have
to be real functions too. Therefore, `when` and `defn` don't count, as they're
macros (the game will tell you so). If you get stuck, a hint shows up after
your second guess: the namespace and the docstring with the name blanked out.
When the round is over you get to see the function itself, with its arglists,
docstring and a link to clojuredocs.

Answers can come from anywhere on clojuredocs, so you might get something from
`clojure.math` or `clojure.zip`. If that feels unfair, switch to `clojure.core`
only in the settings. That's also where you pick the theme. It's all
[Rosé Pine](https://rosepinetheme.com), and by default it follows your system:
Dawn when it's light, the main Rosé Pine palette when it's dark.

Your stats and streaks are saved in the browser. Giving up halfway through a
game counts as a loss.

## Usage


You need Java 11+ and the [Clojure CLI](https://clojure.org/guides/install_clojure).

The game is one ClojureScript file that runs directly in
the browser with [Scittle](https://github.com/babashka/scittle) and Reagent.

```bash
clojure -M:dictionary   # generates the dictionary 
python3 -m http.server -d resources/public 8080 # serves resources/public and locates the game at http://localhost:8080
```

## Where the words come from

`src/clordle/dictionary.clj` downloads the
[clojuredocs export](https://clojuredocs.org/clojuredocs-export.json) and keeps
a var only if two checks agree it's a function. First, clojuredocs has to list
it as a `"function"`. Then the JVM has to agree: the var has to resolve with
`requiring-resolve`, it can't be a special form or a macro, and its value has
to pass `fn?`. That's why the `:dictionary` alias pulls in core.async,
core.logic and a few others, so their vars can actually be loaded and checked.

From there it keeps the names made only of letters, counts them by length and
goes with the most common length. Everything of that length that isn't a
function gets saved too, which is how the game knows why `when` was rejected.

The last run (November 2, 2026, on Clojure 1.12.6) went like this:

| | |
|---|---|
| vars on clojuredocs | 1572 |
| marked as a function by clojuredocs | 1225 |
| also confirmed by the JVM | 1221 |
| names made only of letters | 375 |
| **most common length** | **4 letters, 85 functions** (5 letters: 69, 6 letters: 66) |
