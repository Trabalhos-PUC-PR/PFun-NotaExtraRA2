#!/bin/bash

if [[ "$#" == 0 ]]; then
    exec ghci
fi

# This is needed to run the `main` function by default.
{ echo :cd src ; echo :load "$@" ; echo :!clear; echo main; cat - ; } | exec ghci 