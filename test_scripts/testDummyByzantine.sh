#!/bin/bash
source `dirname $0`/deps

t=30

echo running 4 dummy replicas for $t seconds
 java -cp ${cp} example.DummyByzantineRunner -id 0 $* &
 java -cp ${cp} example.DummyByzantineRunner -id 1 $* &
 java -cp ${cp} example.DummyByzantineRunner -id 2 $* &
 java -cp ${cp} example.DummyByzantineRunner -id 3 $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
