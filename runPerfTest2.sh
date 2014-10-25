#!/bin/bash
source deps

t=300

echo running 4 replicas for $t seconds
java -cp ${cp} example.PerfTest2 -id 0 $* &
java -cp ${cp} example.PerfTest2 -id 1 $* &
java -cp ${cp} example.PerfTest2 -id 2 $* &
java -cp ${cp} example.PerfTest2 -id 3 $* &
sleep $((t + 2))
echo stopping ...
pkill --parent $$
sleep 1
