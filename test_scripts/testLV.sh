#!/bin/bash
source `dirname $0`/deps

t=60

echo running 3 LV replicas for $t seconds
 java -cp ${cp} example.PerfTest2 -id 0 -lv -to 10 --conf src/test/resources/3replicas-conf.xml $* &
 java -cp ${cp} example.PerfTest2 -id 1 -lv -to 10 --conf src/test/resources/3replicas-conf.xml $* &
 java -cp ${cp} example.PerfTest2 -id 2 -lv -to 10 --conf src/test/resources/3replicas-conf.xml $* &
sleep $((t + 2))
echo stopping ...
pkill --parent $$
sleep 1
