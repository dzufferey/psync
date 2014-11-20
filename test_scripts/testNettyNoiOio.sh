#!/bin/bash
source `dirname $0`/deps

t=20

echo running netty test for $t seconds
 java -cp ${cp} tests.Netty 4444 &
 java -cp ${cp} tests.Netty 4445 4444 &
 java -cp ${cp} tests.Netty 4446 4444 &
sleep $((t + 1))
echo stopping ...
pkill --parent $$
sleep 1

echo running nio test for $t seconds
 java -cp ${cp} tests.Nio 4444 &
 java -cp ${cp} tests.Nio 4445 4444 &
 java -cp ${cp} tests.Nio 4446 4444 &
sleep $((t + 1))
echo stopping ...
pkill --parent $$
sleep 1

echo running oio test for $t seconds
 java -cp ${cp} tests.Oio 4444 &
 java -cp ${cp} tests.Oio 4445 4444 &
 java -cp ${cp} tests.Oio 4446 4444 &
sleep $((t + 1))
echo stopping ...
pkill --parent $$
sleep 1
