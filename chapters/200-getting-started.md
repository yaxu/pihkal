## Installation

To install TidalCycles, you will need a laptop or desktop computer,
running Linux, MacOS X or Windows. Your computer doesn't need to be
particularly powerful, but you might well need full admin rights to
it. All components of the TidalCycles system are free/open source. For
the latest installation information, please refer to the instructions
on
[https://tidalcycles.org/Installation](https://tidalcycles.org/Installation).

## Architecture of a Tidal environment

While your installation script is running, let's pause to reflect on
the different parts of a full Tidal environment -- the Tidal library,
the Haskell language, the editor, SuperCollider, SuperDirt, and how
they all fit together. This will later help with imagining what is
going on behind the scenes when you are typing in your Tidal patterns.

The diagram below shows all the different bits, and how they
fit together and communicate. *Haskell* is a general purpose
programming language, which *Tidal* is written in. The program that
runs Haskell is called *ghci*. 

![Diagram of a Tidal environment](./figures/diagram2.pdf){ width=100% }

When you're writing Tidal patterns, you're writing Haskell code, using
the Tidal library. Tidal is a bit more than an add-on for Haskell
though, it provides its own operators and a computational model for
dealing with Patterns, so is really a language in its own right. In
computer science terms, it's a domain specific language, embedded in
Haskell. In turn, there is a "mini-notation" for describing sequences
embedded in Tidal. Tidal does all the pattern generation itself - it
turns the code you write into messages that are sent to a sound
synthesiser, most often *SuperDirt*.

Just as Tidal is written in Haskell, SuperDirt is written in
SuperCollider. SuperCollider is a programming environment for audio
synthesis and digital signal processing (DSP) in
general. SuperCollider is amazing - you'll find SuperCollider under
the hood of a lot of audio live coding environments. In fact, many
people use it as a great live coding system in its own right. If you
like, and are a superhero, you can live code synthesisers and effects
in SuperCollider while live coding patterns to trigger them in Tidal.

With Tidal making patterns, and SuperDirt making sound, the only thing
left is a text editor to work in. There are a few editors that have
plugins for talking with Tidal - atom, vscode, emacs or vim. Whichever
you choose, the plugin will take care of starting Haskell for you,
loading the Tidal library, and setting up the connections with
SuperDirt. 

## Starting up a Tidal environment

Once everything is installed, it's time to start things up.

Normally, you'd start by starting SuperDirt inside SuperCollider, and
then starting Tidal inside your text editor. 

### Starting SuperDirt

Here's a good way to configure SuperCollider to start SuperDirt:

1. Start the SuperCollider application (the system that SuperDirt runs in)
2. Open the 'File' menu then click on 'Open startup file'
3. In that file, paste in `SuperDirt.start`, and then save (File -> Save in the menus)

You've now configured SuperDirt to start whenever you open
SuperCollider. So, if you close the supercollider application and
start it again, SuperDirt should automatically open for you.

SuperDirt has a lot of configuration options, that you can put in the
same startup file. We'll cover those in detail in chapter xxx.

### Starting Tidal, running your first pattern.

Starting Tidal should just be a case of typing some code into your
editor, and running it. A default Tidal installation will be
configured to use the *atom* editor, but it's much the same deal
whatever editor you're using.

1. Start atom
2. Open (or create and save) a file with the `.tidal` extension (e.g. `mylovelypatterns.tidal`).
3. Type or paste in some code (e.g. `d1 $ sound "bd sn"`)
4. Running the code, by making sure the cursor is on it, and pressing
   *shift-enter* or *control-enter*

*shift-enter* runs a single line of code, and *ctrl-* (or on a mac,
*cmd-*) *enter* will run a pattern that runs over multiple lines.

\StoryNote{If you're running multiple lines of code (with
`ctrl-enter`), you can still only run one pattern at a time. Make sure
there's a blank line above and below the pattern you want to run.
}{testa}

When you want to stop the sound, you can replace the pattern with
silence by running this: `d1 $ silence`, or just `hush`{.haskell} by itself.

## Structure of a Tidal pattern

Now you know how to start and stop a pattern, lets jump ahead and look
at a more complicated example. The aim here isn't to understand
everything, but to start to get an idea about what a pattern looks
like, and what tidal is capable of. Here we go:

```{.haskell render="audio" prefix="d1 $ "}
chunk 4 (hurry 2) $ sound "bd [~ rs] mt [lt ht]" # crush 5
```

Running the above, you should start hearing a shifting drum
pattern. Again, to stop it, run this:

```{.haskell}
d1 $ silence
```

or this:

```{.haskell}
hush
```

What just happened? There's already quite a lot to take in here, but
lets have a look at the different bits, working from right to left.

```{.haskell}
crush 5
```

This is a *control pattern*. Here it sets SuperDirt's *bit crusher*
audio effect on, using the constant value `5`{.haskell}. This adds some fairly
subtle distortion to the sound output (try lower values for more
distortion). You can pattern these effects too, we'll come to that in
chapter xxx.

Reading back some more, we find another control pattern, setting the
*sound* that's played.

```{.haskell}
sound "bd [~ rs] mt [lt ht]"
```

This time the value is in speech marks, which means that it's
specified using Tidal's flexible *mini-notation* for sequences. The
words inside - `bd`, `rs`, `mt` etc, are all names of sample banks
(`bd` is short for bass drum, `rs` for rimshot, and `lt`, `mt` and
`ht` for low, mid and high toms). We'll start looking at mini-notation
syntax including `[]`{.haskell} and `~`{.haskell} in the next chapter, but for now lets
just say that it's all about rhythm.

\sloppypar
Lets think about what the `sound` function actually does. It takes
`"bd [~ rs] mt [lt ht]"`{.haskell}, which is a _pattern of words_, and turns it
into a _pattern of sounds_. That is, it takes one kind of pattern as
input, and returns another kind of pattern as output. In Tidal,
everything either tends to be a pattern, or a function for working on
patterns. **It's patterns all the way down.**

You might have noticed that between the `sound` and `crush` control
patterns, there's a `#` character:

```{.haskell}
sound "bd [~ rs] mt [lt ht]" # crush 5
```

The job of `#` is to join the two patterns together, in this case the
`sound` and the `crush` patterns. Super simple to use, but underneath
there are some complexities about how values inside the patterns are
matched up and combined. We'll look into those in chapter xxx.

Reading further back, we see this construction:

```{.haskell}
chunk 4 (hurry 2)
```

This is a funky bit of code, which adds a lot of rhythmic variety by
shifting along, progressively 'speeding up' a quarter of a pattern per
cycle. Again, we'll look at these patterning functions in detail
later, but for now think of this as a machine that takes a pattern as
input, and returns a mangled version of that pattern as output.

```{.haskell}
d1 $
```
Reading right back to the start, we get to `d1`{.haskell}. `d1`{.haskell} is another function, which takes a pattern of controls as input (in this case `sound` and `crush` control patterns combined), and sends it to the synthesiser to be turned into the actual sounds
you can hear. The `$`{.haskell} operator is there to divide up the line; whatever is on the
right of the `$`{.haskell} is calculated before being passed to the function on the left. Again, we'll get more familiar with the usefulness of `$`{.haskell} later on. Looking at the whole pattern again, you can see there's actually two `$`{.haskell}s in it. One makes sure the `sound`{.haskell} and `crush`{.haskell} controls are combined before being mangled by the `chunk`{.haskell} function, and the other makes sure everything gets worked out before finally being passed to `d1`{.haskell}.

```{.haskell render="audio" prefix="d1 $ "}
chunk 4 (hurry 2) $ sound "bd [~ rs] mt [lt ht]" # crush 5
```

That completes our tour of this particular pattern. It'll take a while
to really get your head around all of this, but don't worry, we'll
cover it all again properly later. Next, we go back to basics to have
a proper look at the mini-notation.

