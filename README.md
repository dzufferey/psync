# Round

Fault-tolerant distributed systems play an important role in many critical applications.
However, concurrency, uncertain message delays, and the occurrence of faults make those systems hard to design, implement, and verify.
Round is a framework for writing and verifying high-level implementations of fault-tolerant distributed algorithms.
Round provides communication-closed rounds as primitive, which both simplifies the implementation of the fault-tolerant systems, and makes them more amenable to automated verification.

##

This version is an older version to perform verification tests.

In the current version, the verification is out-of-sync with the frontend:
-the signature of mailbox changed from `Set[(T, ProcessID)]` to `Map[ProcessID, T]`
-the heuristics for handling quantifiers are currently being reworked


## Compiling

This project requires java 7.
You can build it using [sbt](http://www.scala-sbt.org/).
To install sbt follow the instructions at [http://www.scala-sbt.org/release/tutorial/Setup.html](http://www.scala-sbt.org/release/tutorial/Setup.html).

Then, in a console, execute:
```
$ sbt
> compile
> test:compile
```
After the first compilation you should execute `test_scripts/generateClassPath.sh`.
If the dependencies changes you may need to re-generate the class path.
Now you are ready to use te scripts in the `test_scripts` folders.

