#!/bin/bash
source `dirname $0`/deps

t=30

echo running 4 dummy replicas for $t seconds
 java -cp ${cp} example.byzantine.DummyByzantineRunner -id 0 --protocol TCP_SSL $* &
 java -cp ${cp} example.byzantine.DummyByzantineRunner -id 1 --protocol TCP_SSL $* &
 java -cp ${cp} example.byzantine.DummyByzantineRunner -id 2 --protocol TCP_SSL $* &
 java -cp ${cp} example.byzantine.DummyByzantineRunner -id 3 --protocol TCP_SSL $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
