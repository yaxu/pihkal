# Mini notation

We've already seen that Tidal can be broken down into two parts: a
mini-notation for quickly describing sequences, and a library of
functions for transforming pattern. In this chapter, we focus on the
mini-notation. Built on Tidal's flexible approach to musical time, the
mini-notation is a quick way to express rhythms, whether you're making
canonical techno or far-out polyrhythmic minimalism.

## Sequences and sub-sequences

The mini-notation is all about _sequencing_, describing how one event
follows another, in repeating, looping structures. Whenever you see
something in speech marks (`""`{.haskell}), that will almost always be a
mini-notation sequence. Here's a simple example:

```{.haskell render="audio" prefix="d1 $ "}
sound "kick snare"
```

The above plays kick after snare after kick, one after the other,
forever. The `"kick snare"`{.haskell} represents the repeating
mini-notation sequence, the `sound`{.haskell} specifies that it's a
pattern of sounds, and the `d1 $`{.haskell} sends the pattern to be
turned into sound.

\StoryNote{A note for experienced programmers - mini-notation
sequences are immediately parsed into tidal patterns, so although they
\emph{look} like strings, you can't treat them as such.}{strings}

Most examples in this chapter will be visual, rather than musical, so
you can have a good look at the pattern next to its code. Some
examples will visualise patterns of words from left to right, like
this:

```{.haskell render="part" width="1500"}
"kick snare"
```

Others will show patterns of words as colours:

```{.haskell render="gradient" width="1500"}
"orange purple green"
```

Sometimes, I'll show colour patterns as a circle, clockwise from the
top:

```{.haskell render="colour" width="1500"}
"orange purple green"
```

Patterns will most often be visualised as words, as they're
unambiguous, and accessible to colourblind people. I will use colour
patternings from time to time though, and give at least one solid
musical example for each concept.

### Cycle-centric time

This will depend on your cultural background, but in most music
software, musical time is based on the _beat_. Whether you're using a
software sequencer or writing sheet music, you'll generally express
things relative to a tempo (musical speed) measured in _beats per
minute_. In Tidal, things tend to be measured in _cycles_, not
beats. In musical terms, a cycle is equivalent to a _measure_ or
_bar_. What does this mean in practice?

First of all, you'll notice that the more events you add to a
mini-notation sequence, the faster it is played. Compare these two:

```{.haskell render="audio" prefix="d1 $ "}
sound "kick snare clap clap"
```

```{.haskell render="audio" prefix="d1 $ "}
sound "kick snare clap clap bd bd"
```

The latter goes 1.5 times faster than the former, to fit all the
events into a single cycle. 

In other software, you might define a number of beats per bar, and set
the tempo in beats per minute (BPM). In Tidal though, you set cycles
(bars) per second, and the temporal structure within a cycle is
fluid - beats can fall all over the place, with structure coming from
complex and compound ratios rather than a strict metrical grid.

So in Tidal, the _cycle_ is the reference point for patterning, and
not the event. That doesn't mean that things _have_ to fit inside a
cycle, or that one cycle has to be the same as the next.

You can change the current tempo with the `setcps`{.haskell} function,
for example, to play at a rate of 0.4 cycles per second:

```{.haskell}
setcps 0.45
```

If you had four events in a cycle, that would feel like (0.45 * 4 =)
1.8 beats per second, or (0.45 * 4 * 60 =) 108 beats per minute.

### Rests (gaps) with `~`{.haskell}

The 'tilde' token `~`{.haskell} leaves a step empty, creating a
musical rest (gap):

```{.haskell render="part" width=1500}
"a b ~ c"
```

The above pattern still has four 'steps' of equal length, but the
third step is left empty. Here's an audio equivalent:

```{.haskell render="audio"}
sound "kick snare ~ clap"
```

### Subsequences with `[]`

Events don't have to be of equal, though. The following still has four
steps, but the second step contains a _subsequence_, denoted with
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

The following illustrates what this structure looks like as a colour
cycle, clockwise from the top:

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

If you want a step within a sequence to play faster, you can use `*`{.haskell}
followed by a speed factor. For example:

```{.haskell render="audio" prefix='d1 $ '}
sound "bd rs*3 mt lt"
```

The above is still a four step sequence, but the second one is played
three times as fast, so that the rimshot sound is heard three times in
the space of one. The following sounds exactly the same:

```{.haskell render="audio" prefix='d1 $ '}
sound "bd [rs rs rs] mt lt"
```

From the following visual representation, we can clearly the division
into four steps, with the second step 'sped up':

```{.haskell render="part" width=1500}
"bd rs*3 mt lt"
```

Just as `*`{.haskell} speeds up a step, the symbol for divide,
`/`{.haskell}, slows a step down:

```{.haskell render="audio" prefix='d1 $ '}
sound "bd rs/3 mt lt"
```

As a result, you now only hear the rimshot every third step. Lets have
a look at a diagram of this pattern, but sped up by a factor of three
with `[]*3`{.haskell}, so that we see three cycles' worth of the
pattern as a subsequence:

```{.haskell render="part"}
"[bd rs/3 mt lt]*3"
```

You can see that we get a different third of the `rs` event each time
around; the shaded part of each event is the 'active' part. We only
hear a sound when the first third of it plays, because a sound is only
triggered at the _start_ of an event.

\StoryNote{When events get cut into parts like this, the \emph{whole} sound
is triggered when (and only when) the \emph{first} part of the event
plays. This is a little counter-intuitive, but will start to make more
sense when we look at combining patterns together in chapter
xxx. We'll also look at fun ways of properly chopping up sounds into
bits in chapter xxx. }{trigger}

These modifiers can be applied to a subsequence too. If you slow down
a subsequence with three elements in it, by a factor of three, you
will hear one of them per cycle:

```{.haskell render="audio" prefix='d1 $ '}
sound "bd [rs cp ht]/3 mt lt"
```

In other words, you hear one third of the subsequence each time, and
the next time around, it carries on where it left off. Lets have a
look at three cycles worth of that (this time making use of a
function, `fast`{.haskell}):

```{.haskell render="part" width=1500}
fast 3 "bd [rs cp ht]/3 mt lt"
```
Spreading three events over three cycles is straightforward, but what
if the numbers aren't so easily divisible? The answer is, things start
sounding funky. Here's an example with those three events spread over
four cycles:

```{.haskell render="audio" prefix='d1 $ '}
sound "bd [rs cp ht]/4 mt lt"
```

Lets have a look four cycles worth of that:

```{.haskell render="part"}
fast 4 "bd [rs cp ht]/4 mt lt"
```

You can see that Tidal does a good job of splitting the sequence in
four, so that you end up with fragments of events. Remember that a
sound is only triggered by the _start_ of an event, so the first time
around we hear a rimshot at the start of the second step in the
subsequence, the second time a clap one third of the way into the
step, the third time a high tom two thirds into the step, and the
fourth time we don't hear anything during that step - we only get the
tail end of the high tom, which doesn't trigger anything.

### Polyphony

In music, _polyphony_ simply means that two or more sounds can happen
at the same time. There are a lot of ways to layer things up in Tidal,
but in the mini-notation there is really just one way - separating
sequences with commas. There are a few different ways to match up
events in the different subsequences, though.

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

You can see that the two subsequences are squashed to fit the cycle.

### Layering `[]`{.haskell} polyrhythm vs `[]`{.haskell} polymetre

So far we have seen (and heard) that when there are multiple
subsequences inside square brackets, they are layered on top of each
other, with cycles aligned. Lets start with a simple visual example:

```{.haskell render="part" width=1500}
 "[a b c, d e]"
```

When you have two rhythms on top of each other, such as three against
two above, it's known as a _polyrhythm_.

If we replace the square brackets with curly brackets `{}`{.haskell},
then instead the _steps_ align:

```{.haskell render="part" width=1500}
 "{a b c, d e}"
```

The first subsequence has remained the same, but the steps in the
second subsequences now line up with the steps in the first. Because
there aren't enough steps in the second sequence, it loops round. It
is clearer what is going on if we speed up the whole thing by a factor
of three, in order to see three cycles of the mini-notation sequence:

```{.haskell render="gradient" width=1500}
fast 3 "{pink red purple, lightblue darkblue}"
```

This kind of construction, where you layer up sequences with the same
step duration but with differing number of steps, is known as
_polymetre_.

Here's what happen if we change that pattern from curly to square brackets:

```{.haskell render="gradient" width=1500}
fast 3 "[pink red purple, lightblue darkblue]"
```

So to recap, square brackets allow you to create _polyrhythms_ where
subsequences repeat at the same rate but can have different rhythmic
structures, and curly brackets allow _polymetre_, with the same
rhythmic structure, but different periods of repetition.

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

Whereas `[]`{.haskell} is placed around each subsequence,
`.`{.haskell} is placed _between_ successive sequences. This is
sometimes nice to use, but you can't 'nest' subpatterns inside
patterns with `.`{.haskell} alone. You can mix and match them, though:

```{.haskell render="part" width=1500}
 "a b c . d [e f g] . h i"
```

## One step per cycle with `<>`{.haskell}

There is one more pair of symbols for denoting subsequences:
`<>`{.haskell}, also known as angle brackets. These simply slow a
subsequence down to one step per cycle.

```{.haskell render="audio" prefix="d1 $ "}
sound "<lt ht mt>"
```

The angle brackets slow down a subsequence by the number of steps in
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
step. So, these three examples all produce exactly the same result:

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

In the above, the first subsequence is stretched to take up the space
of two steps, and the second the space of three steps. That makes five
in total, so the two subsequences take up two fifths and three fifths
of a cycle respectively.

## Random choices with `?` and `|`

Randomness provides a quick way to introduce variety into a
sequence. We'll cover randomness in detail in chapter xxx, but lets
have a quick look at making random choices within the mini-notation,
right now.

A way to randomly skip playing a step is by using the question mark
(`?`{.haskell}). By default, there will be a 50% chance of an event
playing or not. In the following, the second and fourth steps will be
silent, roughly half the time:

```{.haskell render="audio" prefix="d1 $ "}
sound "bd sd? bd cp?"
```

If a step contains a subsequence, then the randomness will be applied
individually to the steps within:

```{.haskell render="audio" prefix="d1 $ "}
sound "bd [mt ht lt ht]?"
```

It also works with 'sped up' events, for example the eight repetitions
of `bd`{.haskell} in the second step here will be silenced at random:

```{.haskell render="audio" prefix="d1 $ "}
sound "cp bd*8?"
```

Lets see what randomness _looks_ like:

```{.haskell render="colour"}
"orange*24? [[black blue grey]?]*8"
```

You can make an event more, or less likely to play by adding a decimal
number between 0 (never play) and 1 (always play). For example, the
`orange`{.haskell} segments in the following will be removed at
random, around 90% of the time:

```{.haskell render="colour"}
"orange*100?0.9"
```

The `|`{.haskell} character is used in a similar way to the comma
(`,`{.haskell}) in that it separates subsequences. However, instead of
layering them up, it picks one of them to play at random, each cycle.

```{.haskell render="audio" prefix="d1 $ "}
sound "bd [mt|ht lt ht]"
```

Sometimes the above will play the equivalent of `bd mt`{.haskell}, and
others it will play `bd [ht lt ht]`{.haskell}. Here's a visual
example:

```{.haskell render="colour"}
"[white blue|yellow orange red]*16"
```
