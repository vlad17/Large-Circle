# Largest Circle

This is a small project I'm doing in order to learn Haskell.

## System requirements

`ghc` with `cabal`, version 1.16+

## How to use

In the project's root folder:

```bash
$ cabal configure --enable-tests
$ cabal install -j`nproc` --only-dependencies
$ cabal build
$ cabal test
$ ./dist/build/LargeCircle/LargeCircle
```

## Project intent

The project demonstrates the performance of various approximate search algorithms. Currently, I am only planning to test how well genetic algorithms work.

The problem is given by a random assortment of randomly-sized circles, and the task is to find the largest circle which does not intersect with any of the fixed circles. In this sense we are searching the space of all circles which do not intersect with the fixed circles for the one with the largest size.

This project is a direct answer to the challenge posed by [a random page that I came across](http://www.ai-junkie.com/ga/intro/gat3.html).

I suppose I'm also going to try to get through some of the 99 Haskell problems as well.