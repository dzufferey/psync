#!/bin/bash
source deps

#exec java -cp ${cp} -Dio.netty.leakDetectionLevel=advanced example.Main $*
exec java -cp ${cp} example.Main $*
