#!/bin/bash

export PATH=$PATH:`pwd`/sbt

echo
echo Runtime
echo =======

echo
echo decompressing the source code
echo -----------------------------

read -p "Press [Enter] to start ..."

if [ -d psync ]
then
    true
elif [ -f sources/psync-runtime.zip ]
then
    unzip sources/psync-runtime.zip
else
   echo "ERROR: sources/psync-runtime.zip not found"
   exit 1
fi

cd psync

echo
echo compiling
echo ---------

read -p "Press [Enter] to start ..."

sbt compile

echo
echo "running tests (the compilation of the test will produce a fair amount of warnings)"
echo "-------------"

read -p "Press [Enter] to start ..."

sbt test

echo
echo generating classpath dependencies
echo ---------------------------------

read -p "Press [Enter] to start ..."

if command -v gawk >/dev/null 2>&1
then
    ./test_scripts/generateClassPath.sh
else
    echo gawk not found. skipping this optional step.
fi

echo
echo packaging
echo ---------

read -p "Press [Enter] to start ..."

sbt test:package

sbt assembly

cp target/scala-2.11/psync-assembly-0.2-SNAPSHOT.jar .
cp target/scala-2.11/psync_2.11-0.2-SNAPSHOT-tests.jar .

cd ..

echo
echo Verification
echo ============

echo
echo decompressing the source code
echo -----------------------------

read -p "Press [Enter] to start ..."

if [ -d psync-old_verification ]
then
    true
elif [ -f sources/psync-verification.zip ]
then
    unzip sources/psync-verification.zip
else
    echo "ERROR: sources/psync-verification.zip not found"
    exit 1
fi

cd psync-old_verification

echo
echo compiling
echo ---------

read -p "Press [Enter] to start ..."

sbt compile

echo
echo "running tests (the compilation of the test will produce a fair amount of warnings)"
echo "-------------"

read -p "Press [Enter] to start ..."

sbt test

echo
echo generating classpath dependencies
echo ---------------------------------

read -p "Press [Enter] to start ..."

if command -v gawk >/dev/null 2>&1
then
    ./test_scripts/generateClassPath.sh
else
    echo gawk not found. skipping this optional step.
fi

echo
echo packaging
echo ---------

read -p "Press [Enter] to start ..."

sbt test:package

sbt assembly

cp target/scala-2.11/round-assembly-0.1-SNAPSHOT.jar .
cp target/scala-2.11/round_2.11-0.1-SNAPSHOT-tests.jar .

cd ..

echo
echo Done.
echo 
