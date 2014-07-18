#!/bin/bash
source deps

exec java -cp ${cp} example.PerfTest -id 0 $* &
exec java -cp ${cp} example.PerfTest -id 1 $* &
exec java -cp ${cp} example.PerfTest -id 2 $* &
exec java -cp ${cp} example.PerfTest -id 3 $* &
