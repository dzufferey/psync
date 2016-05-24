#!/bin/bash
source `dirname $0`/deps

t=64

echo running 3 LV replicas for $t seconds
 java -cp ${cp} example.batching.BatchingClient -id 0 --conf src/test/resources/3replicas-conf.xml --packetSize 4096 $* &
 java -cp ${cp} example.batching.BatchingClient -id 1 --conf src/test/resources/3replicas-conf.xml --packetSize 4096 $* &
 java -cp ${cp} example.batching.BatchingClient -id 2 --conf src/test/resources/3replicas-conf.xml --packetSize 4096 $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
