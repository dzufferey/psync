#!/bin/bash
source deps

echo running 4 replicas for 20 seconds
java -cp ${cp} example.PerfTest -id 0 $* &
java -cp ${cp} example.PerfTest -id 1 $* &
java -cp ${cp} example.PerfTest -id 2 $* &
java -cp ${cp} example.PerfTest -id 3 $* &
sleep 20
echo stopping ...
pkill --parent $$
