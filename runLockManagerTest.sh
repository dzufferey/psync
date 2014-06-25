#!/bin/bash
export JAVA_OPTS="-Xmx1536M -Xms256M -Xss64M"
scala_version1=2.11
scala_version2=2.11.1
BASEDIR=`dirname $0`

dep0="$BASEDIR/target/scala-${scala_version1}/*"
dep1="$HOME/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-${scala_version2}.jar"
dep2="$HOME/.ivy2/cache/org.scala-lang.modules/scala-xml_${scala_version1}/bundles/*"
dep3="$HOME/.ivy2/cache/org.apache.commons/commons-lang3/jars/*"
dep4="$HOME/.ivy2/cache/io.netty/netty-all/jars/*"

cp="$dep0:$dep1:$dep2:$dep3:$dep4"

#sbt compile
#sbt test:compile
exec java -cp ${cp} example.Main $*
