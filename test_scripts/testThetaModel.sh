#!/bin/bash
source `dirname $0`/deps

t=60

echo running 4 θ-model replicas for $t seconds
 java -cp ${cp} example.TmRunner -id 0 $* &
 java -cp ${cp} example.TmRunner -id 1 $* &
 java -cp ${cp} example.TmRunner -id 2 $* &
 java -cp ${cp} example.TmRunner -id 3 $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
