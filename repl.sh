#!/bin/sh

# Use rlwrap for all repl work
exec rlwrap -S '>> ' janet latex.janet
