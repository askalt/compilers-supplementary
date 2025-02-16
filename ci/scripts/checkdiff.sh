#!/bin/bash

# Script checks a diff between two refs: $SRC, $DST,
# looking for diff like $DIFF among changed files.
# If diff is found returns 1.

if git diff --name-only $SRC $DST | grep $DIFF; then
    echo "Sources are changed"
    exit 1
else
    echo "Sources are unchanged"
    exit 0
fi
