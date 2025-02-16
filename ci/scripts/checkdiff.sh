#!/bin/bash

# Scripts checks a diff between two refs: $SRC, $DST,
# looking for diff like $DIFF among changed files.
#
# Writes `changed` to the $OUT.

if git diff --name-only $SRC $DST | grep $DIFF; then
    echo "Sources are changed"
    echo "changed=true" >> $OUT
else
    echo "Sources are unchanged"
    echo "changed=false" >> $OUT
fi
