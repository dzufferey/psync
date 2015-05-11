# Round

Fault-tolerant distributed systems play an important role in many critical applications.
However, concurrency, uncertain message delays, and the occurrence of faults make those systems hard to design, implement, and verify.
Round is a framework for writing and verifying high-level implementations of fault-tolerant distributed algorithms.
Round provides communication-closed rounds as primitive, which both simplifies the implementation of the fault-tolerant systems, and makes them more amenable to automated verification.

## Example

Starting with an algorithm from the literature which usually looks like that [Charron-Bost, Schiper 09](http://infoscience.epfl.ch/record/159550/files/HO.pdf?version=1):
```
1: Initialization:
2:      x_p := v_p   // v_p is the initial value of p
3: Round r :
4:   S_p :
5:      send x_p to all processes
6:   T_r :
7:      if | HO (p, r) | > 2n/3 then
8:          x_p := the smallest most often received value
9:          if more than 2n/3 values received are equal to x then
10:             decide( x )
```

For the cost of turning the algorithm it into a sightly different syntax, Round provide a distributed runtime and tools to automatically check the correctness of the algorithm.
The same consensus algorithm that runs in Round looks like:
```scala
abstract class IO {
  val initialValue: Int
  def decide(value: Int): Unit
}

class OTR extends Algorithm[IO] {

  val x = new LocalVariable[Int](0)

  def process(id: ProcessID, io: IO) = p(new Process(id) {
            
    def init(io: IO) {
      x <~ io.initialValue
    }

    val rounds = Array[Round](rnd(new Round{

        type A = Int
       
        def send(): Set[(Int, ProcessID)] =
          broadcast(x)
       
        def update(mailbox: Set[(Int, ProcessID)]) {
          if (mailbox.size > 2*n/3) {
            val v = minMostOftenReceived(mailbox)
            x <~ v
            if (mailbox.filter(msg => msg._1 == v).size > 2*n/3)
              io.decide(v)
        } }

)})})}
```

The client code that uses a such algorithm is:
```scala
  //setup the service
  val option: round.runtime.RuntimeOptions = ??? //the list of peers and other parameters
  val algorithm = new OTR
  val rt = new RunTime(alg, options, defaultHandler(_))
  rt.startService

  //run the algorithm for one decision
  val init = Random.nextBoolean
  val io = new ConsensusIO {
    val initialValue = Random.nextInt
    def decide(value: Int) {
      Console.println("decision is " + value)
    }
  }
  rt.startInstance(0, io) //the 1st parameter is the ID of the instance

  //an handler for unexpected messages
  def defaultHandler(msg: Message) {
    ???
    msg.release //unused messages must be released
  }

  //graceful shutdown
  rt.shutdown
```
A given `RunTime` can run many instances in parallel.
However, the ID must be different for each instance.

Complete working examples are located in `src/test/scala/example`.


## Status

Round is in development.

Currently the runtime is fairly stable.
However, we are still woking to improve the performance,

The verfication part is in an early stages and currently only works on a small set of examples.
Currently broken (while we move toward a more robust encoding of the mailbox w.r.t the cardinality constraints).


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


## Running the LockManager example

To easily run the example in an UNIX-like environment, you can use the `test_scripts/runLockManagerTest.sh` script.
By default the configuration file use is the one in `src/test/resources/sample-conf.xml`.
The default configuration uses 4 replicas running on the local host.

* For the replicas, in four different consoles run the following commands:
  - `./test_scripts/runLockManagerTest.sh -id 0 -p 8888`
  - `./test_scripts/runLockManagerTest.sh -id 1 -p 8889`
  - `./test_scripts/runLockManagerTest.sh -id 2 -p 8890`
  - `./test_scripts/runLockManagerTest.sh -id 3 -p 8891`

* To spawn a client and connect in to the first replica:
  `./test_scripts/runLockManagerTest.sh -c -p 9000 -ra 127.0.0.1 -rp 8888`
  The client will then send one request each time you press the enter key.

The scripts depends on `test_scripts/deps` for getting the appropriate dependencies in the classpath.
The script should work for a \*nix machine.
Depending on your OS and configuration, you may need to update this file.

## Assembly and deployement

To simplify the deployement of this project, we use the [sbt-assembly](https://github.com/sbt/sbt-assembly) plugin to produce a single jar containing all the dependencies.
Run `sbt assembly` to produce a this jar.

