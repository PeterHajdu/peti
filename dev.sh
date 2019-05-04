#!/usr/bin/env sh

fwa *.idr | while read; do idris -p iterm -p contrib ./Main.idr -o p; idris -p iterm -p contrib -p specdris ./Test.idr -o test; ./test; echo -------------------; done
