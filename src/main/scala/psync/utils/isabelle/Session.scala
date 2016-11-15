package psync.utils.isabelle

// inspired by
// https://github.com/larsrh/libisabelle/blob/master/examples/src/main/scala/Hello_PIDE.scala
// https://github.com/fthomas/libisabelle-example/blob/master/src/main/scala/libisabelle/example/Main.scala

import psync.formula._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import info.hupel.isabelle._
import info.hupel.isabelle.api._
import info.hupel.isabelle.setup._
import info.hupel.isabelle.pure.{Type => IType, _}
import info.hupel.isabelle.ProverResult._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object Session {

  final val version = Version("2016")
  final val timeout = Duration.Inf
  final val _10s = Duration(10, SECONDS)

  def await[T](a: Awaitable[T], to: Duration = timeout): T = {
    Await.result(a, to)
  }

}

class Session {

  import Session.{version,await}

  protected var system: System = null

  protected val logLevel = Notice
  //protected val logLevel = Info

  var timeout = Session.timeout

  def start {
    Logger.assert(system == null, "isabelle.Session", "session has already started")

    Logger("isabelle.Session", logLevel, "Starting Isabelle")
    val setup = Setup.default(version) match {
      case Left(err) =>
        sys.error(err.toString)
      case Right(setup) =>
        setup
    }
    Logger("isabelle.Session", logLevel, "Setup done")

    val resources = Resources.dumpIsabelleResources() match {
      case Left(err) =>
        sys.error(err.toString)
      case Right(r) =>
        r
    }
    import java.nio.file.Paths
    val paths = List(Paths.get("src/main/isabelle"))
    val config = resources.makeConfiguration(paths, "PSync")
    val env = await(setup.makeEnvironment)
    Logger("isabelle.Session", logLevel, "Environement done")
    Logger("isabelle.Session", logLevel, "Building session")
    if (!System.build(env, config)) {
      Logger.logAndThrow("isabelle.Session", Error, "Build failed")
    } else {
      Logger("isabelle.Session", logLevel, "Starting " + version + " instance")
      system = await(System.create(env, config))
      Logger("isabelle.Session", logLevel, "Isabelle started")
    }
  }

  def stop {
    Logger.assert(system != null, "isabelle.Session", "session has already ended")
    Logger("isabelle.Session", logLevel, "Stopping Isabelle")
    await(system.dispose)
    system = null
  }

  /* hello world operation to test the system */
  def hello = {
    val response = runCommand(Operation.Hello, "world")
    response.unsafeGet
  }

  protected def runCommand[I, O](op: Operation[I, O], arg: I) = {
    Logger.assert(system != null, "isabelle.Session", "session not yet started or already ended")
    val future = system.invoke(op)(arg)
    await(future, timeout)
  }

  def newTheory(name: String) = {
    Logger("isabelle.Session", logLevel, "new theory " + name)
    runCommand(Operations.startTheory, "PSync" -> name)
  }

  /* get the current state of Isabelle */
  def getCurrentState = {
    ???
  }

  def lemma(name: String,
            hypotheses: Seq[Formula],
            conclusion: Formula,
            proof: Option[String]) = {
    lemmaWithFiniteUniverse(name, Nil, Map.empty, hypotheses, conclusion, proof)
  }
  
  def lemmaWithFiniteUniverse(
            name: String,
            finite: Seq[Type],
            namedUniverseCard: Map[Variable,Type],
            hypotheses: Seq[Formula],
            conclusion: Formula,
            proof: Option[String]) = {
    Logger.assert(namedUniverseCard.values.forall(finite contains _),
        "isabelle.Session", "some named universe size are not finite.")
    import TranslateFormula.{universe,mkApp}
    val fs = finite.map(TranslateFormula.finite)
    val naming = namedUniverseCard.map{ case (v,t) =>
        val c = mkApp(Cardinality, universe(t))
        mkApp(Eq, TranslateFormula(v), c)
      }.toSeq
    val hyps = hypotheses.map(TranslateFormula(_))
    val conc = TranslateFormula(conclusion)
    tryLemma(name, fs ++ naming ++ hyps, conc, proof)
  }
  
  protected def tryLemma(name: String,
                         hypotheses: Seq[Term],
                         conclusion: Term,
                         proof: Option[String]) = {
    //XXX do something with the name on the isabelle side ?
    Logger("isabelle.Session", logLevel, "trying to prove " + name)
    val lem = hypotheses match {
      case _ :: _ :: _  =>
        val hs = TranslateFormula.mkApp(And, hypotheses:_*)
        TranslateFormula.mkApp(Implies, hs, conclusion)
      case h :: Nil => TranslateFormula.mkApp(Implies, h, conclusion)
      case Nil => conclusion
    }
    Logger("isabelle.Session", Debug, "isabelle formula: " + lem)
    runCommand(Operations.prove, lem -> proof)
  }

  def prettyPrint(t: Term): String = {
    runCommand(Operations.prettyPrint, t) match {
      case Success(str) => str
      case other => Logger.logAndThrow("isabelle.Session", Error, other.toString)
    }
  }

  def prettyPrint(t: Formula): String = {
    prettyPrint(TranslateFormula(t))
  }

  //TODO commands and stuffs

}
