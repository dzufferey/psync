#!/bin/bash
source deps

exec java -cp ${cp} example.ConsensusVerifier $*
