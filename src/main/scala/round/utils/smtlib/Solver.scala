package round.utils.smtlib

import round.formula._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import scala.sys.process._
import java.io._
import scala.collection.mutable.{HashSet, Stack}

class Solver(th: Theory, cmd: String, options: Iterable[String], implicitDeclaration: Boolean = true) {

  //TODO SMTLIB does not support overloading
  //TODO refactor to produce Command

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
  toSolver("(set-logic "+th+")")

  //default declarations
  declaredT ++= Theory.sort(th)
  declaredS ++= Theory.fun(th).map(t => (t, Nil))

  ///////////////
  ///////////////

  override def finalize {
    try {
      solver.exitValue
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
  }
  
  protected def toSolver(cmd: Command) {
    Logger("smtlib", Debug, "> " +cmd)
    Printer(solverInput, cmd)
    solverInput.newLine
    solverInput.flush
  }

  protected def fromSolver: String = {
    if (solverError.ready) {
      val acc = new StringBuilder()
      while(solverError.ready) {
        acc.append(solverError.readLine)
        acc.append("\n")
      }
      Logger.logAndThrow("smtlib", Error, "solver returned:\n" + acc)
    } else {
      val res = solverOutput.readLine
      Logger("smtlib", Debug, "< " + res)
      res
    }
  }

  def exit = {
    toSolver(Exit)
    try {
      solver.waitFor
      solverInput.close
      solverOutput.close
      solverError.close
    } finally {
      if (!released) {
        SysCmd.release
        released = true
      }
    }
  }
  
  def declare(t: Type) = {
    toSolver(DeclareSort(Names.tpe(t), Names.tpeArity(t)))
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
      case UnInterpretedFct(f, t, p) =>
        Logger.assert(t.isDefined, "smtlib", "declaring sym with unknown type: " + f)
        val name = Names.overloadedSymbol(s, params)
        val tpe = s.instanciateType(params)
        toSolver(DeclareFun(name, tpe))
      case Eq =>
        ()
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
  
  def checkSat: Option[Boolean] = {
    toSolver(CheckSat)
    fromSolver match {
      case "sat" => Some(true)
      case "unsat" => Some(false)
      case "unknown" => None
      case other => Logger.logAndThrow("smtlib", Error, "checkSat: solver said " + other)
    }
  }

  def test(f: Formula): Option[Boolean] = {
    push
    assert(f)
    val res = checkSat
    pop
    res
  }

}

object Solver {

  def setCmd(cmd: Array[String]) = {
    solver = cmd.head
    solverArg = cmd.tail
  }

  var solver = "z3"
  var solverArg = Array("-smt2", "-in")

  def apply(th: Theory, implicitDeclaration: Boolean = true) = {
    new Solver(th, solver, solverArg, implicitDeclaration)
  }

}
