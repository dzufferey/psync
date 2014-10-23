#!/bin/bash
source deps

echo running 4 replicas for 60 seconds
java -cp ${cp} example.PerfTest2 -id 0 $* &
java -cp ${cp} example.PerfTest2 -id 1 $* &
java -cp ${cp} example.PerfTest2 -id 2 $* &
java -cp ${cp} example.PerfTest2 -id 3 $* &
sleep 62
echo stopping ...
pkill --parent $$
sleep 1
