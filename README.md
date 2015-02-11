# Markov Chain Generation

I took a day and made this little Haskell program my brief escape from the living nightmare of *having responsibilities!*

## Background
In statistics and mathematical modeling a *Markovian process* is any
process where
the n+1th state only depends on the nth state. In this way the process
is "memoryless."
For instance, consider the following state sequence:
```
Hungry -> Eating -> Not Hungry
```
Being `Not Hungry` in this case has nothing to do with the fact that
you were ever hungry;
the fact that you were just eating is the only relevant factor. Markov
models are extremely
useful in a number of situations (especially computational
linguistics), partially because
of their simplicity. To learn more, wikipedia has a
[number](http://en.wikipedia.org/wiki/Markov_property)
[of](http://en.wikipedia.org/wiki/Markov_process)
[good](http://en.wikipedia.org/wiki/Hidden_Markov_model)
[articles](http://en.wikipedia.org/wiki/Markov_model) on the subject.

We will specifically be looking at *Markov chains* of text. Consider
the following body
of text (the seed to a Markov model):
> "I am good today. Today is a good day."

We could view this text as the result of a Markovian process: each
word is a state,
depending only on the previous state. We call this result a
[Markov chain](http://en.wikipedia.org/wiki/Markov_chain)
Under this assumption we find that each state (word) has certain
probabilities describing
what the next word will be. For instance state "I" has a 100% chance
of transitioning to
state "am", but state "good" has a 50% probability of transitioning to
"today" and a 50% probability
of transition to "day".

## Program

The program reads in a file one line at a time and uses [Data.Map](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html) to map each word in a line to the list of words that have followed it. The parser deals with strings of short words like "the", "it", and "wow" as single words. If a line of text said

> "Now it is burrito time, and that is good!"

The resulting map would have
```
"now" -> ["it is"]
"it" -> ["is"]
"is" -> ["burrito", "good"]
...
```

The Markov Chain itself is manipulated entirely via God's grandest invention: [The State Monad](https://hackage.haskell.org/package/mtl-2.0.1.0/docs/Control-Monad-State-Lazy.html)
