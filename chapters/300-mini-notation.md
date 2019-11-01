# Mini notation

We've already seen that Tidal can be broken down into two parts: a
library of functions for transforming pattern, and our focus in this
chapter -- a mini-notation for quickly describing sequences. Built on
Tidal's flexible approach to musical time, the mini-notation is a
quick way to express rhythms, whether you're making canonical techno
or far-out polyrhythmic minimalism.

## Sequences and sub-sequences

Whenever you see something in speech marks (`""`{.haskell}), that'll
almost always be a mini-notation sequence. Here's a simple example:

```{.haskell render="audio" prefix="d1 $ "}
sound "kick snare"
```

The above plays kick after snare after kick, one after the other,
forever. 

### Cycle-centric time

This will depend on your cultural background, but in electronic music
and Western classical music musical time is generally based on the
musical _beat_. Whether you're using a software sequencer or writing
sheet music, you'll generally express things relative to a tempo
(musical speed) measured in _beats per minute_. In Tidal, things tend
to be measured in _cycles_, not beats. In musical terms, a cycle is
equivalent to a _measure_ or _bar_. What does this mean in practice?

First of all, you'll notice that the more events you add to a pattern,
the faster it goes. Compare these two:

```{.haskell render="audio" prefix="d1 $ "}
sound "kick snare clap clap"
```

```{.haskell render="audio" prefix="d1 $ "}
sound "kick snare clap clap bd bd"
```

The latter goes 1.5 times faster than the former, to fit all the
events into a single cycle. 

This doesn't mean that things _have_ to fit inside a cycle, or that
one cycle has to be the same as the next. That isn't true at all. What
this means is that in Tidal, the cycle is the reference point for
patterning, and not the event.

In other software you might define a number of beats per bar, and set
the tempo in beats per minute. In Tidal though you set cycles (bars)
per second, and temporal structure within a cycle is fluid - beats can
fall all over the place, with structure coming from complex and
compound ratios rather than a strict metrical grid.

### Rests (gaps) with `~`{.haskell}

The 'tilde' token `~`{.haskell} leaves a step empty, creating a
musical rest (gap):

```{.haskell render="audio" prefix="d1 $ "}
sound "kick snare ~ clap"
```

The above pattern still has four 'steps' of equal length, but the
third step is left empty.

### Subsequences with `[]`

Events don't have to be of equal length though. The following still
has four steps, but with the step is a _subsequence_, denoted with
square brackets:

```{.haskell render="audio" prefix="d1 $ "}
sound "kick [snare bd] ~ clap"
```

So now `kick` and `clap` each take up a quarter of a cycle, and
`snare` and `bd` each take up an eigth of a cycle. If we draw out the
cycle from left to right, the structure looks like this:

```{.haskell render="part" width=1500}
"kick [snare bd] ~ clap"
```

The following
illustrates what this structure looks like as a colour cycle,
clockwise from the top:

```{.haskell render="colour"}
"darkblue [lightblue grey] ~ black"
```

The subsequences can be subdivided however you like. The following has
two steps of half a cycle each, the first one having a subsequence of
three steps, and the second of four steps:

```{.haskell render="colour"}
"[darkblue blue lightblue] [purple red orange yellow]"
```

You can also have subsequences inside subsequences, to any level of depth.

```{.haskell render="colour"}
"[red [blue green] orange] [[red [pink grey] yellow] purple]"
```

### Speeding up and slowing down

If you want to a step within a sequence to play faster, you can use
`*` with a speed factor. For example:

```{.haskell render="audio" prefix='d1 $ '}
sound "bd rs*3 mt lt"
```

The above is still a four step sequence, but the third one is played
three times as fast, so that the rimshot sound is heard three times in
the space of one. The following sounds exactly the same:

```{.haskell render="audio" prefix='d1 $ '}
sound "bd [rs rs rs] mt lt"
```

Similarly, the symbol for divide, `/`, slows a step down:

```{.haskell render="audio" prefix='d1 $ '}
sound "bd rs/3 mt lt"
```

As a result, you now only hear the rimshot every third step. Lets have
a look at a diagram of this pattern, but sped up by a factor of three
with `[]*3`, so that we see three cycles' worth of the pattern as a
subsequence:

```{.haskell render="part"}
"[bd rs/3 mt lt]*3"
```

You can see that we get a different third of the `rs` event each time
around; the shaded part of each event is the 'active' part. We only
hear a sound when the first third of it plays, because a sound is only
triggered at the start of an event.

These modifiers can be applied to a subsequence too. If you slow down
a subsequence with three elements in it, you will hear one of them per
cycle:

```{.haskell render="audio" prefix='d1 $ '}
sound "bd [rs cp ht]/3 mt lt"
```

In other words, you hear one third of the subsequence each time, and
the next time around, it carries on where it left off.

Spreading three events over three cycles is straightforward, but what
if the numbers aren't so easily divisible? The answer is, things start
sounding funky. Here's an example with those three events spread over
four cycles:

```{.haskell render="audio" prefix='d1 $ '}
sound "bd [rs cp ht]/4 mt lt"
```

Lets have a look at that:

```{.haskell render="part"}
"[bd [rs cp ht]/4 mt lt]*4"
```

You can see that Tidal does a good job of splitting the sequence in
four, so that you end up with fragments of events. Remember that a
sound is only triggered by the _start_ of an event, so the first time
we hear a rimshot at the start of the second step in the subsequence,
the second time a clap one third of the way into the step, the third
time a high tom two thirds into the step, and the fourth time we don't
hear anything during that step - we get the tail end of the high tom,
which doesn't trigger anything.

### Polyphony

In music, _polyphony_ simply means that two or more notes or other
events can sound at once. There are a lot of ways to layer things up
in Tidal, but in the mini-notation there is really just one way -
separating sequences with commas. There are a few variants for how the
events in the different subsequences get matched up, though.

If we stick with the square brackets used above, then the sequences
get layered, so that their cycles match up perfectly.

So if we have a simple pattern of tom patterns ...
```{.haskell render="audio" prefix="d1 $ "}
sound "lt ht mt"
```
... and a pattern of rimshots ...

```{.haskell render="audio" prefix="d1 $ "}
sound "[rs rs] [rs rs rs]"
```

... we can play them at the same time by putting a comma between them,
and wrapping the lot in square brackets:

```{.haskell render="audio" prefix="d1 $ "}
sound "[lt ht mt, [rs rs] [rs rs rs]]"
```

Here's how that looks in diagram form:

```{.haskell render="part" width=1500}
 "[lt ht mt, [rs rs] [rs rs rs]]"
```

### Layering `[]`{.haskell} polyrhythm vs `[]`{.haskell} polymetre

So far we have seen and heard that when there are multiple
subsequences inside square brackets, they are layered on top of each
other, with cycles aligned. Lets see a colour example of that:

```{.haskell render="gradient" width=1500}
 "[pink red purple, lightblue darkblue]"
```

When you have two rhythms on top of each other, such as three against
two above, it's known as a _polyrhythm_.

If we replace the square brackets with curly brackets `{}`{.haskell},
then instead the _steps_ align:

```{.haskell render="gradient" width=1500}
 "{pink red purple, lightblue darkblue}"
```

The first subsequence has remained the same, with the two steps in the
second subsequences lining up with the three steps in the
first. Because there aren't enough steps in the second sequence, it
loops round. It is clearer what is going on if we speed up the whole
thing by a factor of three:

```{.haskell render="gradient" width=1500}
 "[{pink red purple, lightblue darkblue}]*3"
```

This kind of construction, where you layer up sequences with the same
step duration but with differing number of steps, is known as a
_polymetre_.

So to recap, square brackets allow you to create _polyrhythms_ where
subsequences repeat at the same rate but have different rhythmic
structures, and curly brackets allow _polymetre_, with the same
rhythmic structure but different periods of repetition.

There's one more thing to note about polymetre. We have seen that with
`{}`{.haskell}, steps align, and that the number of steps per cycle is
given by the first subsequence. for example the following will take
three steps per cycle, from both subsequences:

```{.haskell render="gradient" width=1500}
 "{pink red purple, lightblue blue darkblue orange}"
```

However you can manually set the number of steps per cycle, by adding
`%` and a number after the closing curly bracket. For example to take twelve steps per cycle:

```{.haskell render="gradient" width=1500}
 "{pink red purple, lightblue blue darkblue orange}%12"
```

## Rhythmic 'feet' with `.`{.haskell}

The `.`{.haskell} (full stop/period) character provides an alternative
to grouping with `[]`{.haskell}. For example this ...

```{.haskell render="part" width=1500}
 "[a b c] [d e f g] [h i]"
```

... does the same as this ...

```{.haskell render="part" width=1500}
 "a b c . d e f g . h i"
```

Whereas `[]`{.haskell} is placed around each part, `.`{.haskell} is
placed between successive parts. This is sometimes nice to use, but
you can't 'nest' subpatterns inside patterns with `.`{.haskell}
alone. You can mix and match them, though:

```{.haskell render="part" width=1500}
 "a b c . d [e f g] . h i"
```

## One step per cycle with `<>`{.haskell}

There is one more pair of symbols for denoting subsequences: `<>`,
known as angle brackets. These simply slow a subsequence down to one
step per cycle.

```{.haskell render="audio" prefix="d1 $ "}
sound "<lt ht mt>"
```

It's the same as slowing down a subsequence by the number of steps in
it, for example the following does the same as the above.

```{.haskell render="audio" prefix="d1 $ "}
sound "[lt ht mt]/3"
```

## Repeating steps with `!`{.haskell}

We've seen that `*`{.haskell} speeds up time _inside_ a step,
effectively causing a step to repeat itself, but squashed in the same
space. `!`{.haskell} instead _duplicates_ steps.

You can see the difference here:

```{.haskell render="part" width="1500"}
 "a*3 b!3"
```

`"a*3"`{.haskell} repeats `a` within the step, and `"b!3"`{.haskell}
repeats `b` as additional steps.

If you write a `!` without a number, it'll simply repeat the previous
step. So, these are the same:

```{.haskell}
 "[a b]!2 c!3"
```

```{.haskell}
 "[a b] ! c ! !"
```

```{.haskell render="part" width="1500"}
 "[a b] [a b] c c c"
```

## Elongating steps with `@`{.haskell}

The `@` symbol is similar to `!`{.haskell}, but instead of repeating a
step, is stretches it out over the given number of steps.

```{.haskell render="part" width="1500"}
 "a b@2"
```

This gets particularly interesting when applied to subpatterns:

```{.haskell render="part" width="1500"}
 "[a b c]@2 [d e]@3"
```

## Random choices with `?` and `|`

Random choice is a quick way to introduce variety into a
sequence. We'll be looking at taming randomness in Tidal as a whole in
detail in chapter xxx, but lets have a quick look at making random
within choices the mini-notation now.

A way to make a step optional is using the question mark. By default,
there'll be a 50% chance of an event playing or not. In the following,
the second and fourth steps will be silent roughly half the time:

```{.haskell render="audio" prefix="d1 $ "}
sound "bd sd? bd cp?"
```

If a step contains a subsequence, then the randomness will be applied
individually to the steps within:

```{.haskell render="audio" prefix="d1 $ "}
sound "bd [mt ht lt ht]?"
```

This is true of 'sped up' events, the eight repetitions of
`bd`{.haskell} in the second step here will be played at random:

```{.haskell render="audio" prefix="d1 $ "}
sound "cp bd*8?"
```

You can make an event more, or less likely to play by adding a decimal
number between 0 (never play) and 1 (always play). For example, the
`bd`{.haskell} in the following will play 80% of the time:

```{.haskell render="audio" prefix="d1 $ "}
sound "bd?0.8"
```

The `|`{.haskell} character works in a similar way to `,`{.haskell} in
that is separates subsequences, but instead of layering them up, it
picks one of them to play at random, each cycle.

```{.haskell render="audio" prefix="d1 $ "}
sound "bd [mt|ht lt ht]"
```

Sometimes the above will play the equivalent of `bd mt`{.haskell}, and
others it will play `bd [ht lt ht]`{.haskell}.

