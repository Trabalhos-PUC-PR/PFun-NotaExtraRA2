#!/bin/bash

# This is needed to run the `main` function and auto quit by default 
{ echo :setup; echo :q ; } | exec ghci
