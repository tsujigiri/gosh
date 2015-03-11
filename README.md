# Gosh! [![Build Status](https://travis-ci.org/tsujigiri/gosh.svg?branch=master)](https://travis-ci.org/tsujigiri/gosh)

The game of Go for the command line

![](http://rausch.io/Screenshot%20from%202015-02-22%2013:40:10.png)

This is work in progress and primarily a project for me to learn
Haskell. So far you can add moves by typing coordinates or type "pass".
Dead groups are removed from the board.


## Usage

In order to run it, you need to have cabal installed. Install all the
dependencies and run the program with:

```bash
cabal update
cabal install --only-dependencies
cabal run
```

## TODO

* Scoring
* Play via local network
* Play via Go servers with APIs (e.g. https://online-go.com/)
* Come up with a better name
