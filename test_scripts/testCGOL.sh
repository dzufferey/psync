#!/bin/bash
source `dirname $0`/deps

t=45

echo running 15 replicas of Conway\'s game of life
 java -cp ${cp} example.CgolRunner -id  0 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id  1 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id  2 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id  3 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id  4 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id  5 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id  6 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id  7 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id  8 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id  9 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id 10 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id 11 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id 12 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id 13 --conf src/test/resources/15replicas-conf.xml $* &
 java -cp ${cp} example.CgolRunner -id 14 --conf src/test/resources/15replicas-conf.xml $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
