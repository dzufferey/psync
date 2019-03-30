#!/bin/bash
source `dirname $0`/deps

t=64

echo running 3 LV replicas for $t seconds
 java -cp ${cp} example.batching.BatchingClient -id 0 --conf src/test/resources/3replicas-conf.xml $* &
 java -cp ${cp} example.batching.BatchingClient -id 1 --conf src/test/resources/3replicas-conf.xml $* &
 java -cp ${cp} example.batching.BatchingClient -id 2 --conf src/test/resources/3replicas-conf.xml $* &
#java '-Dio.netty.leakDetectionLevel=paranoid' '-Dio.netty.leakDetection.targetRecords=25' -cp ${cp} example.batching.BatchingClient -id 0 --conf src/test/resources/3replicas-conf.xml $* &
#java '-Dio.netty.leakDetectionLevel=paranoid' '-Dio.netty.leakDetection.targetRecords=25' -cp ${cp} example.batching.BatchingClient -id 1 --conf src/test/resources/3replicas-conf.xml $* &
#java '-Dio.netty.leakDetectionLevel=paranoid' '-Dio.netty.leakDetection.targetRecords=25' -cp ${cp} example.batching.BatchingClient -id 2 --conf src/test/resources/3replicas-conf.xml $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
