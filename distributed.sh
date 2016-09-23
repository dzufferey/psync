#!/bin/bash

#XXX you may need to update this line to point to the location of the jar files on your machine
cp="binaries/psync-assembly-0.2-SNAPSHOT.jar:binaries/psync_2.11-0.2-SNAPSHOT-tests.jar"

#XXX you may need to update this line to point to the location of the configuration file on your machine
conf=conf/distributed.conf

#XXX you need to update host1/2/3 to corresponds to actual hostname of the machines running PSync
declare -A ids
ids=( ["host0"]="0" ["host1"]="1" ["host2"]="2")
id=${ids[`hostname`]}

t=120

echo running for $t seconds with id $id
java -cp ${cp} example.PerfTest3 -id $id -delay 5000 --conf ${conf} $* -b 300 &
sleep $((t + 6))
echo stopping ...
pkill -P $$
sleep 1
