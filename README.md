# hfd

## About

hfd is a console debugger for flash applications.
It is written in Haskell.

## hfd vs fdb

Flash debugger, included into flex sdk lacks good readline interface.
Also it has very pure support of object properties (at least linux version).
So the idea is to create flash debugger with user friendly interface and good properties support.

## Installation

You need Haskell Platform to install hfd.

    $ cd hfd
    $ cabal update
    $ cabal install

