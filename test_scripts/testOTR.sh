#!/bin/bash
source `dirname $0`/deps

t=60

echo running 4 OTR replicas for $t seconds
 java -cp ${cp} example.PerfTest2 -id 0 $* &
 java -cp ${cp} example.PerfTest2 -id 1 $* &
 java -cp ${cp} example.PerfTest2 -id 2 $* &
 java -cp ${cp} example.PerfTest2 -id 3 $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
