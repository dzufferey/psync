#!/bin/bash
source `dirname $0`/deps

t=60

echo running 3 Two Phase Commit replicas
 java -cp ${cp} example.TpcEvtRunner -id 0 --conf src/test/resources/3replicas-conf.xml $* &
 java -cp ${cp} example.TpcEvtRunner -id 1 --conf src/test/resources/3replicas-conf.xml $* &
 java -cp ${cp} example.TpcEvtRunner -id 2 --conf src/test/resources/3replicas-conf.xml $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
