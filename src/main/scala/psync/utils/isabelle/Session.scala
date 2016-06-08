package psync.utils.isabelle

// from
// https://github.com/larsrh/libisabelle/blob/master/examples/src/main/scala/Hello_PIDE.scala
// https://github.com/fthomas/libisabelle-example/blob/master/src/main/scala/libisabelle/example/Main.scala

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import edu.tum.cs.isabelle._
import edu.tum.cs.isabelle.api._
import edu.tum.cs.isabelle.setup._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object Session {

  val version = Version("2016")
  val timeout = Duration.Inf

  def await[T](a: Awaitable[T]): T = {
    Await.result(a, timeout)
  }

}

class Session {

  import Session._

  protected var system: System = null

  def start {
    Logger.assert(system == null, "Isabelle", "session has already started")
    val resources = Resources.dumpIsabelleResources()
    val config = resources.makeConfiguration(Nil, "Protocol")
    val setup = Setup.defaultSetup(version) match {
      case cats.data.Xor.Left(err) =>
        sys.error(err.toString)
      case cats.data.Xor.Right(future) =>
        await(future)
    }
    val env = await(setup.makeEnvironment)
    system = await(System.create(env, config))
  }

  def stop {
    Logger.assert(system != null, "Isabelle", "session has already ended")
    await( system.dispose)
    system = null
  }

  def hello {
    Logger.assert(system != null, "Isabelle", "session not yet started or already ended")
    val future = system.invoke(Operation.Hello)("world")
    val response = await(future)
    println(response.unsafeGet)
  }

  //TODO commands and stuffs

}
