#!/bin/sh

# Don't enable debugging on CI
debugarg="-:dar- "
if [ "$CI" = "true" ]; then
    debugarg=""
fi

# This runs Gambit from the parent project folder
../../../gambit/bin/gsi $debugarg ../../ ./build.test.scm
