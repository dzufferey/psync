#!/bin/bash

set -x

if [ ! -d "$HOME/z3" ]; then
  wget https://github.com/Z3Prover/z3/releases/download/z3-4.4.1/z3-4.4.1-x64-ubuntu-14.04.zip -O ~/z3.zip
  unzip ~/z3.zip -d ~/z3
fi

PATH="$HOME/z3/z3-4.4.1-x64-ubuntu-14.04/bin/:$PATH"
z3 --version
