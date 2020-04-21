# Haskell syntax

Tidal is a language for making patterns, but it is embedded in another, general purpose programming language called _Haskell_. You don't need to learn Haskell to learn TidalCycles, so you are encouraged to skip any parts of this chapter that don't bring you joy right now. Later on though, you might want to clear up some confusion and come back with a heart full of questions. 

## Organising functions with `$`{.haskell} (or `()`{.haskell})

Tidal is all about functions. A function is anything which takes one or more inputs, and gives you an output. For example the `rev`{.haskell} function takes a pattern as input, and returns a reversed version of it as output. That's basically it, but things get a little more complicated; a function takes multiple inputs, and one (or sometimes more!) of those might be another function. We'll have a look at some of those a little later.

You see the dollar sign `$` a _lot_ in Tidal patterns, and it can take a while to get your head around exactly what they're doing. In fact, besides helping structure code, they don't really do anything! However once you're used to them, they make tidal code quick to write and work with.

So if they don't do anything, what are they for? Basically, for helping organise inputs to functions. Lets have a think about why `$` is used in this simple pattern:

```.haskell
d1 $ sound "bd sd"
```

The `$` sits between `d1`, which is a function, and `sound "bd sd"`, a pattern of sounds. Using the `:t` instruction, we can ask haskell what kind of function `d1` is. If we run `:t d1` from a tidal session, this (or something very similar) will appear in the output buffer:

```.plaintext
d1 :: ControlPattern -> IO ()
```

This means that d1 takes a single `ControlPattern`{.haskell} (i.e. a control pattern) as input, and then does some `IO ()` (which stands for input/output but really means any action in the outside world, in this case outputting some sound) as output. What happens if you try to do without the `$`?

```.haskell
d1 sound "bd sd"
```

If you try to run the above, you'll get an error saying something like "The function 'd1' is applied to two arguments, but its type 'ControlPattern -> IO ()' has only one". Well, that error could be clearer, but what it's trying to say is that you're trying to give both `sound`{.haskell} and `"bd sd"`{.haskell} to `d1`, which is two things, whereas `d1` only wants a single thing. What the `$`{.haskell} or `()`{.haskell} does is work out everything to the right of it first, in this case giving the `"bd sd"`{.haskell} pattern to `sound`{.haskell}. The single result of _that_ (a control pattern) is then given to `d1`{.haskell}.

To confirm things, we can also ask for what _type_ of thing `sound`{.haskell} is with `:t sound`, which says:

```.haskell
sound :: Pattern String -> ControlPattern
```

There we see that `"bd sn"`{.haskell} is read as a `Pattern String`{.haskell}, which `sound`{.haskell} turns into `ControlPattern`{.haskell}, which is exactly what `d1`{.haskell} wants. 

As an alternative, it's possible to get things happening in the right order by using `()`{.haskell} instead of `$`{.haskell}, for example the following will work fine:

```.haskell
d1 (sound "bd sd")
```

The disadvantage of the above is that you have to match opening and closing brackets, which can get difficult in more complicated patterns. The advantage is that it works in more situations. In particular the `$`{.haskell} only works for the _last_ input to a function, as it tries to eat up _everything_ on its right. So often, you'll mix and match `()`{.haskell} and `$`{.haskell}, for example:

```.haskell
d1 $ every 3 (fast 2) $ sound "bd sn"
```

In the above, the `every`{.haskell} function wants three inputs, and in this example gets the number `3`{.haskell}, the function `fast 2`{.haskell}, and the pattern `sound "bd sn"`{.haskell}. The second input has to be wrapped in parenthesis, but for the final input we can use the dollar instead. If we want to make that whole pattern extra-fast, we can do that quickly by passing it all to `fast 4`{.haskell}, with a dollar:

```.haskell
d1 $ fast 4 $ every 3 (fast 2) $ sound "bd sn"
```

The `every` function takes _three_ inputs - a count, a function, and a pattern. The function gets applied to the pattern every time the cycle count is up. Lets have a close look at its type, with `:t every`{.haskell}:

```.haskell
every :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
```

This is again trying to tell us that `every`{.haskell} takes three inputs, and gives one output. First there's a `Pattern Int`, the count (_int_ stands for integer, i.e. a whole number). Next there's `(Pattern a -> Pattern a)` the thing that is done, in particular a function that simply takes a pattern as input, and gives a pattern as output. Examples of such functions would be `rev`{.haskell} (reverse the pattern), or `fast 2`{.haskell} (make the pattern run twice as fast). Then there's `Pattern a`{.haskell}, the input pattern that is being transformed, and finally the ouput `Pattern a`{.haskell}, which is the output -- the transformed pattern. It says `Pattern a`{.haskell}, rather than `Pattern String`{.haskell} or whatever, because `every`{.haskell} will work on _any_ kind of pattern, no matter what the contents of it is.

Ok, that got a little technical. If all this still isn't clear, don't worry -- you'll get more of a feel for it through practice.

### `$` vs `#`

As an aside, it's common for Tidal beginners to mix up `$`{.haskell} and `#`{.haskell}, because they both seem to sort of glue things together in a pattern. They have quite different roles, though. `$`{.haskell} sits between a function its (last) input, and `#`{.haskell} sits between two patterns, and combines them together. That's all!

## Passing effects as 'sections'

## Composing functions with `.`{.haskell}

Sometimes you'll have a function (lets stick with `every`{.haskell}) that lets you do something to a pattern, but you want to do _two_ things, or maybe more.

```.haskell
d1 $ every 3 rev $ sound "bd sd"
```

Lets say you wanted to do the above, but as well as reversing the pattern, you also wanted to speed it up. This is where the `.`{.haskell} operator comes in handy -- it lets you join two functions together into a single function. Here we go:

```.haskell
d1 $ every 3 (fast 2 . rev) $ sound "bd sd"
```

Or with a section as well:

```.haskell
d1 $ every 3 (fast 2 . rev . (# speed 2)) $ sound "bd sd"
```

The `.`{.haskell} operator is kind of _chaining together_ the functions. It creates a new function that passes its input to `(# speed 2)`{.haskell}, passes the result of _that_ to `rev`{.haskell}, and then another `.`{.haskell} passes the result of _that_ to `fast 2`{.haskell}. The `.`{.haskell} function is kind of similar to `$`{.haskell}, but the `$`{.haskell} separates an input from a function, and the `.`{.haskell} joins a function to another function, to create a new function. There!

This all becomes super useful when you start getting more confident with Tidal, but again if it's unclear, don't worry about it for now!

## More Haskell resources

Tidal is embedded in Haskell, and all the syntax explored in this chapter has been about Haskell in general rather that Tidal in particular. If your ears are pricked and you'd like to know more, there are a lot of books and online resources for you. For example, some really enjoy _Learn You A Haskell For Great Good_, available for free at on [learnyouahaskell.com](http://learnyouahaskell.com). Others are less happy with the nature of its humourous tone, and find the Haskell Book suits them better, which you can find via [haskellbook.com](http://haskellbook.com).

However, you really don't need to learn any more Haskell than we've covered in this chapter. Writing Tidal is otherwise generally all about using its functions and mini-notation. Furthermore, general Haskell texts tend to focus a lot on things like lists and monads, which Tidal makes very little use of.
