# Effecting sound with control patterns

So far, we've seen a lot of sound patterns. We learned that the word
`sound`{.haskell} is the name of a function that turns a pattern of
_words_ (like `"bd sd"`{.haskell}) into a pattern of _controls_ (like
`sound "bd sd"`{.haskell}). A _sound_ control is one that defines what
kind of sound to play, in particular which set of samples or
synthesiser notes to choose from. There are many more functions
allowing you to pattern other aspects of sound, such as loudness,
pitch, distortion, panning and filtering. This chapter will introduce
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
sound "bd cp sd mt" # crush "16 3"
```

Now we hear something! Here the `sound`{.haskell} and
`crush`{.haskell} controls have been combined together with the
`#`{.haskell} operator. Lets have a look at what we end up with:

```{.haskell render="part" width=1500 preprocess="show <$>" prepend="d1 $ "}
sound "bd cp sd mt" # crush "16 3"
```

There are four events in the result, even though we only gave two
values to the `crush`{.haskell} control. This is because when combine
patterns with the `#`{.haskell} operator, Tidal will start with
events on the left hand side, and match them up with values on the
right hand side. Note that the first two events have a
`crush`{.haskell} value of 16, and the second two have a value of 3, in line with the values that are active.

Because with the `#`{.haskell} operator, Tidal starts with pattern on
the left hand side, if we change the order of patterns, we'll often
get different results. Lets see what happens when we put our
`crush`{.haskell} pattern first:

```{.haskell render="part" width=1500 preprocess="show <$> " prepend="d1 $ "}
crush "16 3" # sound "bd cp sd mt"
```

This will take a bit more explaining! Comparing both examples above,
the shaded parts are actually the same, but in the second example, it
has 'remembered' the size of the two crush events, because they are
now on the left. Whereas the `bd`{.haskell} in the first example was a
simple event taking up the first quarter of the cycle, in the second
example, it's a *fragment* of a larger event. It still takes up a
quarter of a cycle, but it remembers that it is part of an event that
took up half a cycle.

In the second example above, the `crush`{.haskell} value of
`16`{.haskell} has been split in half, the first half matching with
`bd`{.haskell}, and the second matching with `cp`{.haskell}. Again,
the shaded part shows the half you are left with, in both cases. The
crush `3`{.haskell} event has also been cut in half, between the
`sd`{.haskell} and `mt`{.haskell}.

When it comes to listening to the second example, you only hear the
(`bd`{.haskell}) and (`sd`{.haskell}) sounds, and *not* the
(`cp`{.haskell}) and (`mt`{.haskell}). This is because the first half
of both `cp`{.haskell} and `mt`{.haskell} have been 'cut off', and so
they are never triggered!

This combining of events works even when they don't line up. Here's a
pattern where three events are combined with two:

```{.haskell render="part" width=1500 preprocess="show <$> " prepend="d1 $ "}
sound "bd cp sd" # crush "16 3"
```

With the `#`{.haskell} operator, we keep the structure of the three
events from the left hand side. The middle one gets split in half,
between the two events on the right hand side. In the end, three
events are triggered; `cp`{.haskell} partly matches up with `crush
3`{.haskell}), but because this part doesn't include the beginning of
`cp`{.haskell}, it doesn't trigger a sound.

The below shows that if we swap the `crush`{.haskell} and
`sound`{.haskell} patterns around this time, the event structure now
comes from the `crush`{.haskell}. This time there are only two events
with their 'starts' intact and therefore triggering sounds --
`bd`{.haskell} and `cp`{.haskell}.

```{.haskell render="part" width=1500 preprocess="show <$> " prepend="d1 $ "}
crush "16 3" # sound "bd cp sd""
```

\StoryNote{You might be wondering why events that have lost their trigger point
are kept at all, if they can't be heard. The answer is that these
event fragments become useful when it comes to using patterns in
different ways, such as combining with yet another control pattern, or
feeding into a function that transforms the pattern so fragmentary
event onsets once more come into play.}{lost-triggers}

## Rule of thumb

This has been quite a technical chapter, but in fact, you don't really
have to understand any of these 'mechanics'. Most of the time, you
don't need to worry about how Tidal is working. Instead, you can just
tell Tidal what you want! Using the above as an example, if you want
the second half of a cycle to be more distorted than the first, then
you can just add `# crush "16 3"`{.haskell} to the pattern. If you
prefer, you don't need to worry about the detail, you can just get on
with making music!

However, a good rule of thumb is that with `#`{.haskell}, "structure
comes from the left". If you want the rhythm to come from an effect
like `crush`{.haskell}, put it on the left. If you want it to come
from the `sound`{.haskell} pattern, put that on the left.
