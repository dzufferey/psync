# PSync

![Version 0.3 SNAPSHOT](https://img.shields.io/badge/version-0.3_SNAPSHOT-green.svg)
[![Build Status](https://travis-ci.org/dzufferey/psync.svg?branch=master)](https://travis-ci.org/dzufferey/psync)
[![Apache licensed](https://img.shields.io/badge/license-Apache-blue.svg)](https://raw.githubusercontent.com/dzufferey/psync/master/LICENSE)

Fault-tolerant distributed systems play an important role in many critical applications.
However, concurrency, uncertain message delays, and the occurrence of faults make those systems hard to design, implement, and verify.
PSync is a framework for writing and verifying high-level implementations of fault-tolerant distributed algorithms.
PSync is based on the Heard-Of model and provides communication-closed rounds as primitive, which both simplifies the implementation of the fault-tolerant systems, and makes them more amenable to automated verification.

The name PSync comes from [partial synchrony](http://groups.csail.mit.edu/tds/papers/Lynch/jacm88.pdf) a model for distributed system that makes it possible to provide an illusion of synchrony even though the underlying system may not be always/fully synchronous.

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

For the cost of turning the algorithm it into a sightly different syntax, PSync provide a distributed runtime and tools to automatically check the correctness of the algorithm.
The same consensus algorithm that runs in PSync looks like:
```scala
abstract class IO {
  val initialValue: Int
  def decide(value: Int): Unit
}

class OtrProcess extends Process[IO]{

  var x = 0
  var decision = -1     // for the specification
  var decided = false   // for the specification
  var callback: IO = null

  def init(io: IO) = i{
    callback = io
    x = io.initialValue
  }

  val rounds = phase(
    new Round[Int]{

      def send(): Map[ProcessID,Int] = {
        broadcast(x) //macro for (x, All)
      }

      def update(mailbox: Map[ProcessID,Int]) {
        if (mailbox.size > 2*n/3) {
          x = minMostOftenReceived(mailbox)
          if (mailbox.filter{ case (k, msg) => msg == x }.size > 2*n/3) {
            callback.decide(v)
            decided = true
            decision = v
            exitAtEndOfRound
} ) } } } }
```

The Process are then wrapped in an `Algorithm` which may also contain a specification.
For the one third rule algorithm that solves the consensus problem, it may look like:
```scala
class OTR extends Algorithm[IO, OtrProcess] {

  import SpecHelper._

  val V = new Domain[Int]

  val spec = new Spec {
    val goodRound: Formula = S.exists( s => P.forall( p => p.HO == s && s.size > 2*n/3 ))
    val livenessPredicate = List( goodRound, goodRound )
    val invariants = List[Formula](
      (    P.forall( i => !i.decided )
        || V.exists( v => {
             val A = P.filter( i => i.x == v);
             A.size > 2*n/3 && P.forall( i => i.decided ==> (i.decision == v))
           })
      ) && P.forall( i => P.exists( j1 => i.x == init(j1.x) )),
         V.exists( v => {
            val A = P.filter( i => i.x == v);
            A.size == (n: Int) && P.forall( i => i.decided ==> (i.decision == v))
         })
      && P.forall( i => P.exists( j1 => i.x == init(j1.x) )),
      P.exists( j => P.forall( i => i.decided && i.decision == init(j.x)) )
    )

    val properties = List[(String,Formula)](
      ("Termination",    P.forall( i => i.decided) ),
      ("Agreement",      P.forall( i => P.forall( j => (i.decided && j.decided) ==> (i.decision == j.decision) ))),
      ("Validity",       P.forall( i => i.decided ==> P.exists( j => init(j.x) == i.decision ))),
      ("Integrity",      P.exists( j => P.forall( i => i.decided ==> (i.decision == init(j.x)) ))),
      ("Irrevocability", P.forall( i => old(i.decided) ==> (i.decided && old(i.decision) == i.decision) ))
    )
  }

  def process = new OtrProcess

}
```

The client code that uses a such algorithm is:
```scala
  //setup the service
  val option: psync.runtime.RuntimeOptions = ??? //the list of peers and other parameters
  val rt = new RunTime(options, defaultHandler(_)) //manage the connections, resources, etc.
  rt.startService //open socket, connect to the other peer if using TCP, etc.
  val algorithm = new OTR(tr)

  //run the algorithm for one decision
  val init = Random.nextBoolean
  val io = new ConsensusIO {
    val initialValue = Random.nextInt
    def decide(value: Int) {
      Console.println("decision is " + value)
    }
  }
  algorithm.startInstance(0, io) //the 1st parameter is the ID of the instance

  //an handler for unexpected messages
  def defaultHandler(msg: Message) {
    ???
    msg.release //unused messages must be released
  }

  //graceful shutdown
  rt.shutdown
```
A given `RunTime` can run many instances and algorithms in parallel.
However, the ID must be different for each instance.

Complete working examples are located in `src/test/scala/example`.


## Status

PSync is in development.

Currently the runtime is fairly stable.
However, we are still woking to improve the performance.

The verfication part is in an early stages and currently only works on a small set of examples.
Currently broken (while we move toward a more robust encoding of the mailbox w.r.t the cardinality constraints).


## Compiling

This project requires java 8 or newer.
You can build it using [sbt](https://www.scala-sbt.org/).
To install sbt follow the instructions at [https://www.scala-sbt.org/release/docs/Setup.html](https://www.scala-sbt.org/release/docs/Setup.html).

Then, in a console, execute:
```
$ sbt
> compile
> test:compile
```
After the first compilation you should execute `utils/generateClassPath.sh`.
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

## Assembly and deployment

To simplify the deployment of this project, we use the [sbt-assembly](https://github.com/sbt/sbt-assembly) plugin to produce a single jar containing all the dependencies.
Run `sbt assembly` to produce a this jar.

## Known Issues

When running many tests it is not unusual to see the following warning:

    WARNING: Failed to generate a seed from SecureRandom within 3 seconds. Not enough entropy?

Starting/stopping Netty too many times seems to deplete the pool of entropy.
Currently PSync does not use any cryptographic primitive, therefore, this warning is harmless.

## References

Here are some references about how PSync works.

* [PSync: A Partially Synchronous Language for Fault-Tolerant Distributed Algorithms](http://people.csail.mit.edu/zufferey/files/2016_PSync_A_Partially_Synchronous_Language_for_Fault-Tolerant_Distributed_Algorithms.pdf), Drăgoi et al., POPL 2016
* [The Need for Language Support for Fault-Tolerant Distributed Systems](http://drops.dagstuhl.de/opus/volltexte/2015/5019/), Drăgoi et al., SNAPL 2015
* [A Logic-based Framework for Verifying Consensus Algorithms](http://people.csail.mit.edu/zufferey/files/2014_A_Logic-Based_Framework_for_Verifying_Consensus_Algorithms.pdf), Drăgoi et al., VMCAI 2014
