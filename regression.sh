#!/usr/bin/env bash

try()
{
    local PROG=${1?}; shift
    echo "==== $PROG ===="
    dist/build/$PROG/$PROG "$@" |& diff $PROG.out -
}

try TestTCD Hinkley
try TideConstituents 2014
try TideAmplitudes Hinkley 2014
try Tides Hinkley '1961-05-26 14:29' '1961-05-28 06:38' 01:26
