# Round

a small summary explanation ...

## Compiling

This project is build using [sbt](http://www.scala-sbt.org/).
To install sbt follow the instructions at [http://www.scala-sbt.org/release/tutorial/Setup.html](http://www.scala-sbt.org/release/tutorial/Setup.html).

Then, in a console, execute:
```
$ sbt
> compile
> test:compile
> package
> test:package
```

## Running the LockManager example

To easily run the example in an UNIX-like environment, you can use the `runLockManagerTest.sh` script.
By default the configuration file use is the one in `src/test/resources/sample-conf.xml`.
The default configuration uses 3 replicas running on the local host.

* For the replicas, in four different consoles run the following commands:
  - `./runLockManagerTest.sh -id 0 -p 8888`
  - `./runLockManagerTest.sh -id 1 -p 8889`
  - `./runLockManagerTest.sh -id 2 -p 8890`
  - `./runLockManagerTest.sh -id 3 -p 8891`

* To spawn a client and connect in to the first replica:
  `./runLockManagerTest.sh -c -p 9000 -ra 127.0.0.1 -rp 8888`
  The client will then send one request each time you press the enter key.

