#!/bin/bash
source `dirname $0`/deps

t=5

echo running 3 BenOr replicas
 java -cp ${cp} example.BenOrRunner -id 0 --conf src/test/resources/3replicas-conf.xml $* &
 java -cp ${cp} example.BenOrRunner -id 1 --conf src/test/resources/3replicas-conf.xml $* &
 java -cp ${cp} example.BenOrRunner -id 2 --conf src/test/resources/3replicas-conf.xml $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
