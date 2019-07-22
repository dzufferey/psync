#!/bin/bash
source `dirname $0`/deps

t=60

echo 'running 25 replicas of Dijsktra self-stabilizing mutual exclusion protocol (token ring)'
java -cp ${cp} example.SelfStabilizingRunner -id  0 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id  1 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id  2 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id  3 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id  4 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id  5 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id  6 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id  7 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id  8 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id  9 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 10 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 11 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 12 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 13 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 14 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 15 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 16 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 17 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 18 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 19 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 20 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 21 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 22 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 23 --conf src/test/resources/25replicas-conf.xml $* &
java -cp ${cp} example.SelfStabilizingRunner -id 24 --conf src/test/resources/25replicas-conf.xml $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
