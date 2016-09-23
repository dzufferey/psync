#!/bin/bash

#XXX you may need to update this line to point to the location of the jar files on your machine
cp="binaries/psync-assembly-0.2-SNAPSHOT.jar:binaries/psync_2.11-0.2-SNAPSHOT-tests.jar"

#XXX you may need to update this line to point to the location of the configuration file on your machine
conf=conf/local.conf

t=60

echo running 3 LastVoting with batching replicas for $t seconds
java -cp ${cp} example.PerfTest3 -id 0 --conf ${conf} -b 300 $* &
java -cp ${cp} example.PerfTest3 -id 1 --conf ${conf} -b 300 $* &
java -cp ${cp} example.PerfTest3 -id 2 --conf ${conf} -b 300 $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
