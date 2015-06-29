#!/bin/bash
source `dirname $0`/deps

#to run a program using the same running script on multiple machines

declare -A ids
ids=( ["hostName1"]="0" ["hostName2"]="1" ["hostName3"]="2" ["hostName4"]="3" ) #XXX put the name of the machine on which the system will run

t=60
id=${ids[`hostname`]}
echo "running replica $id for $t seconds"
java -cp ${cp} example.PerfTest -id $id $* & #XXX replace by the program you want to run
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
