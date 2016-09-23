#!/bin/bash

#XXX you may need to update this line to point to the location of the jar files on your machine
cp="binaries/round-assembly-0.1-SNAPSHOT.jar:binaries/round_2.11-0.1-SNAPSHOT-tests.jar"

echo
echo Running the verification on the One-Third-Rule example
echo
echo The result will be saved to report_otr.html
echo

read -p "Press [Enter] to start ..."

java -cp ${cp} example.ConsensusVerifier -r report_otr.html

echo
echo Running the verification on the LastVoting example
echo
echo The result will be saved to report_lv.html
echo

read -p "Press [Enter] to start ..."

java -cp ${cp} example.ConsensusVerifier -lv -r report_lv.html
