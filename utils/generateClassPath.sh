#!/bin/bash
PSYNC_BASE="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

file=test_scripts/deps
scala_version=2.13

echo "#this is the classpath wrt the root directory of the project" > $file
echo -n "cp=\"" >> $file
sbt "show dependencyClasspath" | tr -d ' ' | tr ',' '\n' | gawk 'match($0, /Attributed\(([^)]*)\)/, a) {print a[1]}' | tr '\n' ':' >> $file
echo -n "$PSYNC_BASE/target/scala-${scala_version}/classes:$PSYNC_BASE/target/scala-${scala_version}/test-classes/" >> $file
echo '"' >> $file
echo  >> $file

echo "#with assembly we need only the following two jars:" >> $file
echo "#cp=\"$PSYNC_BASE/target/scala-${scala_version}/psync-assembly-0.2-SNAPSHOT.jar:$PSYNC_BASE/target/scala-${scala_version}/psync_${scala_version}-0.2-SNAPSHOT-tests.jar\"" >> $file
echo  >> $file

echo '# trap CTRL-C input, and kill every process created' >> $file
echo 'trap "pkill -P $$; sleep 1; exit 1;" INT' >> $file
