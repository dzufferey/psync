#!/bin/bash

file=test_scripts/deps

echo "#this is the classpath wrt the root directory of the project" > $file
echo -n "cp=\"" >> $file
sbt "show dependencyClasspath" | tr -d ' ' | tr ',' '\n' | gawk 'match($0, /Attributed\(([^)]*)\)/, a) {print a[1]}' | tr '\n' ':' >> $file
echo -n "target/scala-2.11/classes:target/scala-2.11/test-classes/" >> $file
echo '"' >> $file
echo  >> $file

echo "#with assembly we need only the following two jars:" >> $file
echo "#cp=\"target/scala-2.11/round-assembly-0.1-SNAPSHOT.jar:target/scala-2.11/round_2.11-0.1-SNAPSHOT-tests.jar\"" >> $file

