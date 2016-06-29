package psync.utils.smtlib

import psync.formula._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import scala.sys.process._
import java.io._
import scala.collection.mutable.{HashSet, Stack}
import java.util.concurrent.TimeUnit

abstract class Result
case class Sat(model: Option[Model] = None) extends Result
case object UnSat extends Result
case object Unknown extends Result
case class Failure(reason: String) extends Result

class Solver( th: Theory,
              cmd: String,
              options: Iterable[String],
              implicitDeclaration: Boolean,
              dumpToFile: Option[String],
              timeout: Long,
              incremental: Boolean) {

  protected var stackCounter = 0

  //////////////
  // Plumbing //
  //////////////

  protected val solver = java.lang.Runtime.getRuntime.exec(Array(cmd) ++ options, null, null)
  protected val solverInput = {
    val out = solver.getOutputStream()
    //val out = new org.apache.commons.io.output.TeeOutputStream(solver.getOutputStream(), System.out)
    new BufferedWriter(new OutputStreamWriter(out))
  }
  protected val solverOutput = new BufferedReader(new InputStreamReader(solver.getInputStream()))
  protected val solverError = new BufferedReader(new InputStreamReader(solver.getErrorStream()))

  protected val fileDump = dumpToFile.map( file => new BufferedWriter(new FileWriter(file)) )

  //////////////////
  // Declarations //
  //////////////////

  protected val declaredV = HashSet[Variable]()
  protected val declStack = Stack(Set[Variable]())
  
  protected val declaredS = HashSet[(Symbol, List[Type])]()
  protected val symbolStack = Stack(Set[(Symbol, List[Type])]())
  
  protected val declaredT = HashSet[Type]()
  protected val typeStack = Stack(Set[Type]())

  ////////////////////
  // Initialisation //
  ////////////////////

  Logger("smtlib", Debug, "starting: " + (Array(cmd) ++ options).mkString(" "))
  toSolver("(set-option :print-success false)")
  toSolver("(set-option :produce-models true)")
  toSolver("(set-logic "+th+")")

  //default declarations
  declaredT ++= Theory.sort(th)
  declaredS ++= Theory.fun(th).map(t => (t, Nil))

  ///////////////
  ///////////////

  override def finalize {
    try {
      solver.exitValue
      fileDump.foreach(_.close)
    } catch {
      case _: java.lang.IllegalThreadStateException =>
        solver.destroy
    }
  }

  protected def toSolver(cmd: String) {
    Logger("smtlib", Debug, "> " +cmd)
    solverInput.write(cmd)
    solverInput.newLine
    solverInput.flush
    for (f <- fileDump) {
      f.write(cmd)
      f.newLine
      f.flush
    }
  }
  
  protected def toSolver(cmd: Command) {
    Logger("smtlib", Debug, "> " +cmd)
    Printer(solverInput, cmd)
    solverInput.newLine
    solverInput.flush
    for (f <- fileDump) {
      Printer(f, cmd)
      f.newLine
      f.flush
    }
  }

  protected def fromSolver: String = {

    def reader(stream: BufferedReader) =
      new java.util.concurrent.Callable[String] {
        def call = {
          val acc = new StringBuilder()
          do {
            val line = stream.readLine
            Logger("smtlib", Debug, "< " + line)
            acc.append(line)
            acc.append("\n")
            //for large model we need to wait that the stream refill
            if (!stream.ready) {
              Thread.sleep(1)
            }
          } while(stream.ready)
          acc.toString.trim
        }
      }

    if (solverError.ready) {
      val acc = new StringBuilder()
      while(solverError.ready) {
        acc.append(solverError.readLine)
        acc.append("\n")
      }
      Logger.logAndThrow("smtlib", Error, "solver returned:\n" + acc)
    } else {
      val future = Solver.executor.submit(reader(solverOutput))
      try {
        val res = future.get(timeout, TimeUnit.MILLISECONDS)
        res
      } catch {
        case e: java.util.concurrent.TimeoutException =>
          Logger("smtlib", Warning, "solver timeout.")
          forceExit
          "TIMEOUT"
      }
    }
  }

  def forceExit {
    solver.destroy 
    solverInput.close
    solverOutput.close
    solverError.close
    for (f <- fileDump) f.close
  }

  def exit = {
    toSolver(Exit)
    solver.waitFor
    solverInput.close
    solverOutput.close
    solverError.close
    for (f <- fileDump) f.close
  }
  
  def declare(t: Type) = {
    try {
      toSolver(DeclareSort(Names.tpe(t), Names.tpeArity(t)))
    } catch {
      case e: Exception =>
        //collectTypes returns a bit more than what it should
        Logger("smtlib", Warning, "unsupported type: " + t)
    }
  }

  def typeDecl(t: Type) = {
    val (args, ret) = t match {
      case Function(args, r) => (args, r)
      case other => (Nil, other)
    }
    val argsDecl = args.map(Names.tpe).mkString("("," ",")")
    argsDecl + " " + Names.tpe(ret)
  }

  def declare(f: Formula) = f match {
    case Variable(v) => toSolver(DeclareFun(v, f.tpe))
    case other => Logger.logAndThrow("smtlib", Error, "not supported: " + other)
  }
  
  def declare(sp: (Symbol, List[Type])) = {
    val (s, params) = sp
    s match {
      case Eq => //has a special status
        ()
      case Lt | Leq | Gt | Geq if params == List(Int) => //has a special status
        ()
      case UnInterpretedFct(f, t, p) =>
        Logger.assert(t.isDefined, "smtlib", "declaring sym with unknown type: " + f)
        val name = Names.overloadedSymbol(s, params)
        val tpe = s.instanciateType(params)
        try {
          toSolver(DeclareFun(name, tpe))
        } catch {
          case e: Exception =>
            //collectSymbolsWithParams returns a bit more than what it should
            Logger.logAndThrow("smtlib", Error, "unsupported type: " + name + ": " + tpe + " from " + f + ": " + t + p.mkString("[",",","]") + " and " + params)
        }
      case i: InterpretedFct =>
        val name = Names.overloadedSymbol(s, params)
        val tpe = s.instanciateType(params)
        toSolver(DeclareFun(name, tpe))
    }
  }

  protected def pushOnStack[A](elts: Set[A], stack: Stack[Set[A]], decls: HashSet[A]): Set[A] = {
    val newElts = elts -- decls
    decls ++= newElts
    val frame = stack.pop
    stack.push(frame ++ newElts)
    newElts
  }

  def mkDeclarations(f: Formula) = {
    val newSort = pushOnStack(FormulaUtils.collectTypes(f), typeStack, declaredT)
    newSort foreach declare
    val newSym = pushOnStack(FormulaUtils.collectSymbolsWithParams(f), symbolStack, declaredS)
    newSym foreach declare
    val newVars = pushOnStack(f.freeVariables, declStack, declaredV)
    newVars foreach declare
  }
  
  def assert(f: Formula) {
    if (implicitDeclaration) {
      mkDeclarations(f)
    }
    toSolver(Assert(f))
  }
  
  def push {
    scala.Predef.assert(incremental)
    if (implicitDeclaration) {
      declStack.push(Set[Variable]())
      symbolStack.push(Set[(Symbol, List[Type])]())
      typeStack.push(Set[Type]())
    }
    stackCounter += 1
    toSolver(Push)
  }
  
  def pop {
    scala.Predef.assert(incremental)
    if (implicitDeclaration) {
      declaredV --= declStack.pop
      declaredS --= symbolStack.pop
      declaredT --= typeStack.pop
    }
    Logger.assert(stackCounter > 0, "smtlib", "pop -> stackCounter = " + stackCounter)
    toSolver(Pop)
    stackCounter -= 1
  }
  
  def checkSat: Result = {
    toSolver(CheckSat)
    fromSolver match {
      case "sat" => Sat()
      case "unsat" => UnSat
      case "unknown" => Unknown
      case other =>
        Logger("smtlib", Warning, "checkSat: solver said " + other)
        Failure(other)
    }
  }
  
  def getModel: Option[Model] = {
    toSolver(GetModel)
    Thread.sleep(100) //sleep a bit to let z3 make the model. TODO better!
    Parser.parseModel(fromSolver).map( cmds => {
      Model(cmds, declaredV, declaredS)
    })
  }

  def testB(f: Formula): Boolean = {
    test(f) match {
      case Sat(_) => true
      case UnSat => false
      case _ => sys.error("result is not (un)sat.")
    }
  }

  def test(f: Formula): Result = {
    test(FormulaUtils.getConjuncts(f))
  }

  def test(conjuncts: List[Formula]): Result = {
    conjuncts.foreach(Checks(_))
    if (incremental) push
    conjuncts.foreach(assert(_))
    val res = checkSat
    res match {
      case Failure(_) => //solver might be dead
        try if (incremental) pop catch { case _: Throwable => () }
      case other => if (incremental) pop
    }
    res
  }

  def testWithModel(f: Formula): Result = {
    testWithModel(FormulaUtils.getConjuncts(f))
  }

  def testWithModel(conjuncts: List[Formula]): Result = {
    conjuncts.foreach(Checks(_))
    if (incremental) push
    conjuncts.foreach(assert(_))
    val res = checkSat match {
      case Sat(None) =>
        getModel match {
          case Some(m) => Sat(Some(m))
          case None =>
            Logger("smtlib", Warning, "testWithModel: could not get model")
            Sat()
        }
      case other => other
    }
    res match {
      case Failure(_) => //solver might be dead
        try if (incremental) pop catch { case _: Throwable => () }
      case other => if (incremental) pop
    }
    res
  }

  def getValue(fs: Formula*): Option[List[(Formula,Formula)]] = {
    toSolver(GetValue(fs.toList))
    Parser.parseGetValueReply(fromSolver)
  }

}

object Solver {
  
  //for asyn IO
  val executor = java.util.concurrent.Executors.newCachedThreadPool()

  def setCmd(cmd: Array[String]) = {
    solver = cmd.head
    solverArg = cmd.tail
  }

  var solver = "z3"
  var solverArg = Array("-smt2", "-in")

  val z3Cmd = "z3"
  val z3Args = Array("-smt2", "-in")
  
  val cvc4MfCmd = "cvc4"
  val cvc4MfArgs = Array( "--lang=smt2",
                          "--finite-model-find",
                          "--mbqi=none",
                          "--inst-max-level=0",
                          "--fmf-inst-engine",
                          "--simplification=none")//,
                          //"-i" )

  def z3(th: Theory, file: Option[String], timeout: Long): Solver = {
    new Solver(th, z3Cmd, z3Args, true, file, timeout, false)
  }

  def cvc4mf(th: Theory, file: Option[String], timeout: Long): Solver = {
    new Solver(th, cvc4MfCmd, cvc4MfArgs, true, file, timeout, false)
  }

  def useZ3 {
    solver = z3Cmd
    solverArg = z3Args
  }

  def useCvc4Mf {
    solver = cvc4MfCmd
    solverArg = cvc4MfArgs
  }

  var defaultTO = 10000l

  def apply(th: Theory): Solver = apply(th, None, defaultTO)
  
  def apply(th: Theory, file: String): Solver = apply(th, Some(file), defaultTO)

  def apply(th: Theory, timeout: Long): Solver = apply(th, None, timeout)

  def apply(th: Theory, file: String, timeout: Long): Solver = apply(th, Some(file), timeout)

  def apply(th: Theory, file: Option[String], timeout: Long): Solver = {
    new Solver(th, solver, solverArg, true, file, timeout, false)
  }

}

