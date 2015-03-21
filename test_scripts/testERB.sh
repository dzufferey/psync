#!/bin/bash
source `dirname $0`/deps

t=5

echo running 3 Eager Reliable Broadcast replicas
 java -cp ${cp} example.ERBRunner -id 0 --conf src/test/resources/3replicas-conf.xml $* &
 java -cp ${cp} example.ERBRunner -id 1 --conf src/test/resources/3replicas-conf.xml $* &
 java -cp ${cp} example.ERBRunner -id 2 --conf src/test/resources/3replicas-conf.xml $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
