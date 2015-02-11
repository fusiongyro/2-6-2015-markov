# Markov Chain Generation

I took a day and made this little Haskell program my brief escape from the
living nightmare of *having responsibilities!*

## Background

In statistics and mathematical modeling a *Markovian process* is any process
where the n+1th state only depends on the nth state. In this way the process is
"memoryless."  For instance, consider the following state sequence:

```
Hungry -> Eating -> Not Hungry
```

Being `Not Hungry` in this case has nothing to do with the fact that you were
ever hungry; the fact that you were just eating is the only relevant
factor. Markov models are extremely useful in a number of situations (especially
computational linguistics), partially because of their simplicity. To learn
more, wikipedia has a [number](http://en.wikipedia.org/wiki/Markov_property)
[of](http://en.wikipedia.org/wiki/Markov_process)
[good](http://en.wikipedia.org/wiki/Hidden_Markov_model)
[articles](http://en.wikipedia.org/wiki/Markov_model) on the subject.

We will specifically be looking at *Markov chains* of text. Consider the
following body of text (the seed to a Markov model):

> "I am good today. Today is a good day."

We could view this text as the result of a Markovian process: each word is a
state, depending only on the previous state. We call this result a [Markov
chain](http://en.wikipedia.org/wiki/Markov_chain) Under this assumption we find
that each state (word) has certain probabilities describing what the next word
will be. For instance state "I" has a 100% chance of transitioning to state
"am", but state "good" has a 50% probability of transitioning to "today" and a
50% probability of transition to "day".

## Input Files

This program will run on whatever pile of plain text you tell it to run. I'm
including the [Project Gutenberg](https://www.gutenberg.org/) copies of *Alice's
Adventures in Wonderland*, *Peter Pan*, and *The Wonderful Wizard of Oz*.

The lineup is chosen in the hopes of randomly recreating Alan Moore's totally
fucked up graphic novel [Lost Girls](https://en.wikipedia.org/wiki/Lost_Girls).

On that note, If anyone has a plaintext copy of *Fifty Shades of Grey*, I'm
looking for it!

## Program (Haskell Breakdown)

The program reads in a file one line at a time and uses
[Data.Map](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html)
to map each word in a line to the list of words that have followed it. The
parser deals with strings of short words like "the", "it", and "wow" as single
words. If a line of text said

> "Now it is burrito time, and that is good!"

The resulting map would have
```
"now" -> ["it is"]
"it" -> ["is"]
"is" -> ["burrito", "good"]
...
```

The process of selecting the next word to add to the chain is probabilistic,
which requires random number generation, which in Haskell, means you need to
have a [random
generator](https://hackage.haskell.org/package/random-1.0.0.3/docs/System-Random.html)
which will need to be updated every time a new word needs to be selected to add
to the chain. And when you really think about it, the markov chain itself needs
to be stored and passed around in order to update it.

So, in order to operate on this alarmingly stateful markov chain, I turned my
attention to God's grandest invention: [The State
Monad](https://hackage.haskell.org/package/mtl-2.0.1.0/docs/Control-Monad-State-Lazy.html)

The state of the markov chain can be described by the list of words in the chain
and the next random generator to use:

```
type MarkovState = ([String], StdGen)
```

The function which is used to add words to the chain takes a MarkovState and
returns the `String` added to the chain together with the new `MarkovState`

```
addToChain :: MarkovMap -> MarkovState -> (String, MarkovState)
```

That (after you pass it a Map it) is a stateful computation! They have a Monad
for that!

```
instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState
```

This makes operating on the markov chain totally not a nightmare! In fact, a lot
of the problem is a pleasant breeze once the only tricky part is wrapped up in a
categorical little box that you can call `>>=` on!

For my last trick, I wrote this totally simple function:

```
runMarkovSt :: MarkovState -> MarkovSt -> MarkovState
runMarkovSt start action = snd $ runState action start

```

It takes the current state of a Markov Chain, and a neatly wrapped up stateful
computation, and just returns the new resulting state. Really simple, it's
almost the same as `runState`, which comes for free in `Control.Monad.State`
anyway.

The reason this function exists is because I got the idea that I could chain
together a list of stateful computations, and apply them all to an initial state
one after another. So, if you're willing to do a little bit of type-fu...

```
\_ >>> :t scanl
scanl :: (b -> a -> b) -> b -> [a] -> [b]
```

... You can create use `runMarkovSt` (the `b -> a -> b` Gotham deserves) to
create an infinite list of markov chains!

## ...

And that's why this program is awesome!