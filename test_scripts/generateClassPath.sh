#!/bin/bash

file=test_scripts/deps
scala_version=2.12

echo "#this is the classpath wrt the root directory of the project" > $file
echo -n "cp=\"" >> $file
sbt "show dependencyClasspath" | tr -d ' ' | tr ',' '\n' | gawk 'match($0, /Attributed\(([^)]*)\)/, a) {print a[1]}' | tr '\n' ':' >> $file
echo -n "target/scala-${scala_version}/classes:target/scala-${scala_version}/test-classes/" >> $file
echo '"' >> $file
echo  >> $file

echo "#with assembly we need only the following two jars:" >> $file
echo "#cp=\"target/scala-${scala_version}/psync-assembly-0.2-SNAPSHOT.jar:target/scala-${scala_version}/psync_${scala_version}-0.2-SNAPSHOT-tests.jar\"" >> $file

