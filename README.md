# Markov Chain Generation
This week for fpcoffee we will be generating Markov chains!

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

## Task
Our task is to read a body of text as an input file, (say
[the King James Bible and Structure and Interpretation of Computer Programs](http://kingjamesprogramming.tumblr.com/))
and produce our own Markov chains. Usage would look something like
this:
```
$ markov input_seed.txt
  This is my silly markov chain text. It was statistically generated,
  and makes only
  a small amount of sense!
  ```
  Good luck!

# Submission
To make your own solution and show it off to the group:
1. Fork this repository.
2. Make your own solution in a folder named your git username.
3. Submit a pull request to this repository when you have finished.
Next week (that is 2/13/2015) we will get together to discuss and show
our solutions!
