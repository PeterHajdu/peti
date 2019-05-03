#!/usr/bin/env sh

fwa *.idr | while read; do idris -p iterm -p contrib ./Main.idr -o p; echo -------------------; done
