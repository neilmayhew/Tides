#!/bin/bash

try()
{
    local PROG=${1?}; shift
    echo "==== $PROG ===="
    dist/build/$PROG/$PROG "$@" |& diff $PROG.out -
}

try TestTCD Hinkley
try TideConstituents 2014
try TideAmplitudes Hinkley 2014
try Tides Hinkley 2014-06-13
