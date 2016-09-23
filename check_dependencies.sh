#!/bin/sh

export PATH=$PATH:`pwd`/sbt

check() {
    echo "checking for $1"
    command -v $1 >/dev/null 2>&1 || { echo "  $1 is required but it's not installed"; return; }
    echo "  $1 found"
}

recommend() {
    echo "checking for $1"
    command -v $1 >/dev/null 2>&1 || { echo "  $1 is recommended (but not installed)"; return; }
    echo "  $1 found"
}

check java

check bash

check z3

check sbt

recommend cssh

recommend unzip

recommend gawk

