package psync

import psync.runtime.{Runtime, Message, InstanceHandler, AlgorithmOptions}
import dzufferey.utils.LogLevel._
import dzufferey.utils.Logger
import java.util.concurrent.ArrayBlockingQueue

//use a value class to keep ProcessID separated from short/int in the typechecker
class ProcessID(val id: Short) extends AnyVal

//IO is a type parameter to communicate the initial value, parameter, and callbacks
//the use of mixing composition forces elements (like variables) to be used only with the algorithm
abstract class Algorithm[IO, P <: Process[IO]](runtime: Runtime, opts: AlgorithmOptions = null) extends Specs[IO, P]
{

  //round number
  var r = -1

  //number of processes
  var n = 0

  //the univers of processes
  val P = new Domain[P]
  //val P = new Domain[ProcessID]
  //and sets of Process
  val S = new Domain[Set[ProcessID]]

  //specification of the consensus
  val spec: Spec

  def process: P

  /** A sample IO object. This is currently needed for the verification. */
  def dummyIO: IO

  def startInstance(
      instanceId: Short,
      io: IO,
      messages: Set[Message] = Set.empty): Unit = {
    val p = getProcess
    runtime.startInstance(instanceId, p, io, messages)
  }

  def stopInstance(instanceId: Short): Unit = {
    runtime.stopInstance(instanceId)
  }

  /*******************/
  /* Runtime related */
  /*******************/

  protected[psync] def options = {
    if (opts != null) opts
    else if (runtime != null) runtime.options
    else new AlgorithmOptions{ }
  }

  //TODO try a stack for better locality
  private val processPool = new ArrayBlockingQueue[InstanceHandler[IO,P]](2*options.processPool)

  protected[psync] final def recycle(p: InstanceHandler[IO,P]): Unit = {
    processPool.offer(p)
  }

  private def createProcess: InstanceHandler[IO,P] = {
    assert(runtime != null)
    val p = process
    new InstanceHandler(p, this, runtime)
  }

  private def getProcess: InstanceHandler[IO,P] = {
    val proc = processPool.poll
    if (proc == null) {
      Logger("Algorithm", Warning, "processPool is running low")
      createProcess
    } else {
      proc
    }
  }

  //preallocate
  if (runtime != null) {
    for (i <- 0 until options.processPool) {
      processPool.offer(createProcess)
    }
  }

}

//placeholder for quantifying over some domain
class Domain[A] {
  def forall(fct: A => Boolean): Boolean = sys.error("only for specification purpose, removed by macros") // linter:ignore UnusedParameter
  def exists(fct: A => Boolean): Boolean = sys.error("only for specification purpose, removed by macros") // linter:ignore UnusedParameter
  def filter(fct: A => Boolean): Set[A] =  sys.error("only for specification purpose, removed by macros") // linter:ignore UnusedParameter
}

