#!/usr/bin/env sh

fwa *.idr | while read; do make test; echo -------------------; done
