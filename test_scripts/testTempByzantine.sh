#!/bin/bash
source `dirname $0`/deps

t=64

# trap CTRL-C input, and kill every process created
trap "pkill -P $$; sleep 1; exit 1;" INT

echo running 4 replicas for $t seconds
 java -cp ${cp} example.byzantine.test.Runner -id 0 --conf src/test/resources/sample-conf.xml --protocol TCP_SSL --byzantine 1 $* &
 java -cp ${cp} example.byzantine.test.Runner -id 1 --conf src/test/resources/sample-conf.xml --protocol TCP_SSL --byzantine 1 $* &
 java -cp ${cp} example.byzantine.test.Runner -id 2 --conf src/test/resources/sample-conf.xml --protocol TCP_SSL --byzantine 1 $* &
 java -cp ${cp} example.byzantine.test.Runner -id 3 --conf src/test/resources/sample-conf.xml --protocol TCP_SSL --byzantine 1 $* &
sleep $((t + 2))
echo stopping ...
pkill -P $$
sleep 1
