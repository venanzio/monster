# Monster

*Monster* is a Haskell library on Monadic Streams

A *pure stream* is an infinite sequence of values, for example
```haskell
  nats = 0 <: 1 <: 2 <: 3 <: 4 <: ...
```
The type of pure streams with elements of type a is denoted by *(Stream a)*.
The cons operation *(<:)* is used to append an element in front
of an existing stream.
Stream are infinite, so we must use recursion to define them.
For example, the stream of ones is defined as
```haskell
  ones = 1 <: ones
```
The list of natural numbers can be defined as
```haskell
  nats = fromNat 0
    where fromNat n = n <: fromNat (n+1)
```
  
A *monadic stream* (which we call a *monster* for short)
is a sequence of values in which every constructor (<:)
is guarded by a monadic action: 
to obtain the head (first element) and tail (continuation) of the stream,
we must execute the monadic action.

For example a *Maybe-monster* is a sequence of elements that is either
Nothing (meaning the sequence is finished) or Some head element followed by a tail.
This allows us to define both finite and infinite sequences.
The definitions of `ones` and `nats` are still valid Maybe-monsters.
In addition we can define finite sequences:
```haskell
  1 <: 2 <: 3 <: empty
```
where `empty` is the Maybe-monster given by the Nothing action.

# Outline of Modules

## MonStreams

Definition of the type of monsters `MonStr m a`

Instances of Functor, Applicative, Monad

## Operations

Definition of generalization to monster of functions
defined in Data.List and in the Stream package by Swiestra and van Dijk

## PureStreams

Defines pure streams as Identity-monsters.

Definition of pure and polymorphic stream functions.

## Morphisms

Definition of morphisms of Monads (natural transformation satisfying additional monad equations)
and their lifting to monsters.

## MonStrExamples

Instantiations with various monads: Maybe, List, IO, State.

Some example for testing.

## Other
*PureStreamEquations* and *MonStreamEquations*: a language to define system of pure stream equations and their interpretation/solution in pure streams and monster, respectively. 
(This is an initial implementation of the work leading to the proof of undecidability by Sattler and Balestrieri.)

*MonadStream*: Chris' original instance of Monad for monsters. Now integrated in MonStreams, so not needed any more.

*Stream*: library by Swiestra and van Dijk. Keep it until we finished generalizing all the functions, then remove it.
