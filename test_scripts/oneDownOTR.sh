#!/bin/bash
source `dirname $0`/deps

t=60

echo running 3 out of 4 OTR replicas for $t seconds
 java -cp ${cp} example.PerfTest2 -id 0 -to 10 $* &
 java -cp ${cp} example.PerfTest2 -id 1 -to 10 $* &
 java -cp ${cp} example.PerfTest2 -id 2 -to 10 $* &
# java -cp ${cp} example.PerfTest2 -id 3 -to 10 $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
