#!/bin/bash
source `dirname $0`/deps

exec java -cp ${cp} example.ConsensusVerifier $*
