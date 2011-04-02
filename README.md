# hfd

## About

hfd is a console debugger for flash applications.
It is written in Haskell.

## hfd vs fdb

Flash debugger, included into flex sdk (fdb) lacks good readline interface.
Also it very often fails (and even crashes) when printing properties (at least linux version).
So the idea is to create flash debugger with user friendly interface and good properties support.

## Installation

You need Haskell Platform to install hfd.

    $ cd hfd
    $ cabal update
    $ cabal install

## Current state

hfd supports most of basic debugger features

* breakpoints
* execution control: continue, step, next, finish
* inspect variable
* inspect properties (call getters)
* break on exception
* show call stack
* list source code (currently only around current position)
* haskeline user interface (commands history, basic completion)

Features, that are not implemented still

* improved listing of source code
* walk through call stack
* manage breakpoint (list, delete, enable/disable)
* conditional breakpoints
* expression evaluation
* set variables
* print arguments when printing call stack
* improved completion

