#!/bin/bash
source `dirname $0`/deps
exec java -Xss512M -cp ${cp} tests.FJT $*
