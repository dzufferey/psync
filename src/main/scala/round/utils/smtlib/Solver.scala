package round.utils.smtlib

import round.formula._
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
              dumpToFile: Option[String]) {

  protected var stackCounter = 0

  SysCmd.acquire
  protected var released = false

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
    } finally {
      if (!released) {
        SysCmd.release
        released = true
      }
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

  protected def fromSolver(timeout: Long = 10000): String = {

    def reader(stream: BufferedReader) =
      new java.util.concurrent.Callable[String] {
        def call = {
          val acc = new StringBuilder()
          do {
            acc.append(stream.readLine)
            acc.append("\n")
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
        Logger("smtlib", Debug, "< " + res)
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
    try {
      solver.destroy 
      solverInput.close
      solverOutput.close
      solverError.close
      for (f <- fileDump) f.close
    } finally {
      if (!released) {
        SysCmd.release
        released = true
      }
    }
  }

  def exit = {
    toSolver(Exit)
    try {
      solver.waitFor
      solverInput.close
      solverOutput.close
      solverError.close
      for (f <- fileDump) f.close
    } finally {
      if (!released) {
        SysCmd.release
        released = true
      }
    }
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
    if (implicitDeclaration) {
      declStack.push(Set[Variable]())
      symbolStack.push(Set[(Symbol, List[Type])]())
      typeStack.push(Set[Type]())
    }
    stackCounter += 1
    toSolver(Push)
  }
  
  def pop {
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
    fromSolver() match {
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
    Parser.parseModel(fromSolver()).map( cmds => {
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
    push
    conjuncts.foreach(assert(_))
    val res = checkSat
    pop
    res
  }

  def testWithModel(f: Formula): Result = {
    testWithModel(FormulaUtils.getConjuncts(f))
  }

  def testWithModel(conjuncts: List[Formula]): Result = {
    conjuncts.foreach(Checks(_))
    push
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
    pop
    res
  }

  def getValue(fs: Formula*): Option[List[(Formula,Formula)]] = {
    toSolver(GetValue(fs.toList))
    Parser.parseGetValueReply(fromSolver())
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

  def apply(th: Theory) = {
    new Solver(th, solver, solverArg, true, None)
  }
  
  def apply(th: Theory, file: String) = {
    new Solver(th, solver, solverArg, true, Some(file))
  }

}

