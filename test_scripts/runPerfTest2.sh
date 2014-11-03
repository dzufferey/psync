#!/bin/bash
source `dirname $0`/deps

t=60

echo running 4 replicas for $t seconds
 java -cp ${cp} example.PerfTest2 -id 0 $* &
 java -cp ${cp} example.PerfTest2 -id 1 $* &
 java -cp ${cp} example.PerfTest2 -id 2 $* &
 java -cp ${cp} example.PerfTest2 -id 3 $* &
#java -cp ${cp} example.PerfTest2 -id 0 $* > log.0.txt &
#java -cp ${cp} example.PerfTest2 -id 1 $* > log.1.txt &
#java -cp ${cp} example.PerfTest2 -id 2 $* > log.2.txt &
#java -cp ${cp} example.PerfTest2 -id 3 $* > log.3.txt &
#java '-Dio.netty.leakDetectionLevel=advanced' -cp ${cp} example.PerfTest2 -id 0 $* &
#java '-Dio.netty.leakDetectionLevel=advanced' -cp ${cp} example.PerfTest2 -id 1 $* &
#java '-Dio.netty.leakDetectionLevel=advanced' -cp ${cp} example.PerfTest2 -id 2 $* &
#java '-Dio.netty.leakDetectionLevel=advanced' -cp ${cp} example.PerfTest2 -id 3 $* &
sleep $((t + 2))
echo stopping ...
pkill --parent $$
sleep 1
