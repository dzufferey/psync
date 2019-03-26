#!/bin/bash
source `dirname $0`/deps

t=60

echo 'running 21 out of 25 replicas of an eventually strong failure detector (◇ S)'
 java -cp ${cp} example.EsfdRunner -id  0 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id  1 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id  2 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id  3 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id  4 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id  5 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id  6 --conf src/test/resources/25replicas-conf.xml $* &
#java -cp ${cp} example.EsfdRunner -id  7 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id  8 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id  9 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 10 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 11 --conf src/test/resources/25replicas-conf.xml $* &
#java -cp ${cp} example.EsfdRunner -id 12 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 13 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 14 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 15 --conf src/test/resources/25replicas-conf.xml $* &
#java -cp ${cp} example.EsfdRunner -id 16 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 17 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 18 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 19 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 20 --conf src/test/resources/25replicas-conf.xml $* &
#java -cp ${cp} example.EsfdRunner -id 21 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 22 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 23 --conf src/test/resources/25replicas-conf.xml $* &
 java -cp ${cp} example.EsfdRunner -id 24 --conf src/test/resources/25replicas-conf.xml $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
