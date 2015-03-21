#!/bin/bash
source `dirname $0`/deps

t=15

echo running 7 ε-consensus replicas
 java -cp ${cp} example.EpsilonRunner -id 0 --conf src/test/resources/7replicas-conf.xml $* &
 java -cp ${cp} example.EpsilonRunner -id 1 --conf src/test/resources/7replicas-conf.xml $* &
 java -cp ${cp} example.EpsilonRunner -id 2 --conf src/test/resources/7replicas-conf.xml $* &
 java -cp ${cp} example.EpsilonRunner -id 3 --conf src/test/resources/7replicas-conf.xml $* &
 java -cp ${cp} example.EpsilonRunner -id 4 --conf src/test/resources/7replicas-conf.xml $* &
 java -cp ${cp} example.EpsilonRunner -id 5 --conf src/test/resources/7replicas-conf.xml $* &
 java -cp ${cp} example.EpsilonRunner -id 6 --conf src/test/resources/7replicas-conf.xml $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
