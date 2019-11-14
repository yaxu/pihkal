# Effecting sound with control patterns

So far, we've seen a lot of sound patterns. We learned that the word
`sound`{.haskell} is the name of a function that turns a pattern of
_words_ (like `"bd sd"`{.haskell}) into a pattern of _controls_ (like
`sound "bd sd"`{.haskell}). A _sound_ control is one that defines what
kind of sound to play, in particular which set of samples or
synthesiser notes to choose from. There are many more functions
allowing you to pattern other aspects of sound, such as loudness,
pitch, distortion, panning, filtering. This chapter will introduce
them, and how to combine them together.

## Combining control patterns

Lets start by looking at `crush`{.haskell} control patterns. Here's an
example:

```{.haskell render="audio" prefix="d1 $ "}
crush "16 3"
```

The `crush`{.haskell} function creates a control pattern, just like
`sound`{.haskell}, however its input is a pattern of _numbers_ rather
than words. Also, if you run this code, it doesn't actually make any
sound! `crush`{.haskell} is for applying a 'bitcrushing' distortion
effect, but you won't hear anything until you also give a sound to be
crushed:

```{.haskell render="audio" prefix="d1 $ "}
sound "bd cp sd mt" |+ crush "16 3"
```

Now we hear something! Here the `sound`{.haskell} and
`crush`{.haskell} controls have been combined together with the
`|+`{.haskell} operator. Lets have a look at what we end up with:

```{.haskell render="part" width=1500 preprocess="show <$>" prepend="d1 $ "}
sound "bd cp sd mt" |+ crush "16 3"
```

There are four events in the result, even though we only gave two
values to the `crush`{.haskell} control. This is because when combine
patterns with the `|+`{.haskell} operator, Tidal will start with
events on the left hand side, and match them up with values on the
right hand side. Note that the first two events have a
`crush`{.haskell} value of 16, and the second two have a value of 3, in line with the values that are active .

There's also a `+|`{.haskell} operator, which instead starts with
events from the right-hand side, and matches them up with values on
the left. Lets change our previous example to use that:

```{.haskell render="part" width=1500 preprocess="show <$> " prepend="d1 $ "}
sound "bd cp sd mt" +| crush "16 3"
```

This will take a bit more explaining! Comparing both examples above,
the shaded parts are the same, but the second example has 'remembered'
the the original events on the right. Whereas the `bd`{.haskell} in
the first example was a simple event taking up the first quarter of
the cycle, in the second example, it's a *fragment* of an event. It
still takes up a quarter of a cycle, but it remembers that it is
originally from an event that took up half a cycle.

In the second example above, the `crush`{.haskell} value of
`16`{.haskell} has been split in half, the first half matching with
`bd`{.haskell}, and the second matching with `cp`{.haskell}. Again,
the shaded part shows the half you are left with, in both cases. The
`3`{.haskell} event has also been cut in half, between the
`sd`{.haskell} and `mt`{.haskell}.

When it comes to listening to the above pattern though, you only hear
the first (`bd`{.haskell}) and third (`sd`{.haskell}) sounds, and
*not* the second (`cp`{.haskell}) and fourth (`mt`{.haskell}). This is
because sounds are *only triggered at the start of events*. The first
half of both `cp`{.haskell} and `mt`{.haskell} have been cut off, and
so they are never triggered.

This combining of events works even when they don't line up. Here's a
pattern where three events are combined with two:

```{.haskell render="part" width=1500 preprocess="show <$> " prepend="d1 $ "}
sound "bd cp sd" |+ crush "16 3"
```

With the `|+`{.haskell} operator, we keep the structure of the three
events from the left hand side. The middle one gets split in half,
between the two events on the right hand side. In the end, three
events are triggered; the `cp`{.haskell} has lost its start, so
doesn't play.

The below shows that if we instead use the `+|`{.haskell} operator,
the event structure comes the pattern on the right, and is split
between events on the left. This time there are only two events with
their 'starts' intact (`bd`{.haskell} and `cp`{.haskell}, and that
therefore result in a sound being triggered.

```{.haskell render="part" width=1500 preprocess="show <$> " prepend="d1 $ "}
sound "bd cp sd" +| crush "16 3"
```

There is one more operator in this family, `|+|`{.haskell}. Whereas
`|+`{.haskell} starts with events on the left, and `+|`{.haskell}
starts with events on the right, `|+|`{.haskell} results in events
which 'forget' where they came from. Here it is in practice:

```{.haskell render="part" width=1500 preprocess="show <$> " prepend="d1 $ "}
sound "bd cp sd" |+| crush "16 3"
```

In this final example, all four are 'whole' events, and so you hear
all four.

## Summary

You might be wondering why events are kept at all, if they have lost
their trigger point, and so can't be heard. The answer is that these
event fragments become useful when it comes to using patterns in
different ways, such as combining with yet another control pattern, or
feeding it into a function that transforms the resulting pattern so
fragmentary event onsets once more come into play.

