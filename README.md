# Largest Circle

## System requirements

`ghc` with `cabal`, version 1.18+ (see end for installation instructions).

## How to use

In the project's root folder:

```bash
$ cabal install -j`nproc` --only-dependencies --enable-tests
$ cabal build
$ cabal test
$ ./dist/build/LargeCircle/LargeCircle
```

Note that for some reason the GTK+ signal handler requires two inputs,
so if you want to exit by terminal then send SIGINT twice.

## Project intent

The project demonstrates the performance of various approximate search algorithms. Currently, I am only planning to test how well genetic algorithms work.

The problem is given by a random assortment of randomly-sized circles, and the task is to find the largest circle which does not intersect with any of the fixed circles. In this sense we are searching the space of all circles which do not intersect with the fixed circles for the one with the largest size.

This project is a direct answer to the challenge posed by [a random page that I came across](http://www.ai-junkie.com/ga/intro/gat3.html).

I suppose I'm also going to try to get through some of the 99 Haskell problems as well.

## Installation instructions

Installation instruction on Debian. Non-aptitude package managers
should ~~suck it~~ have their respective invocations.

```bash
$ sudo apt-get install haskell-platform
$ cabal install caball-install
```

## TODO list

1. Add a scoring system for circle solutions
2. Add best-so-far accessor for genetic learners
3. Implement an encoding for largest-circle solutions
4. Clean up the interface 
5. Add unit testing