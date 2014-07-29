package round.utils.smtlib

import round.formula._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import scala.sys.process._
import java.io._
import scala.collection.mutable.{HashSet, Stack}

class Solver(th: Theory, cmd: String, options: Iterable[String], implicitDeclaration: Boolean = true) {

  //TODO SMTLIB does not support overloading

  protected var stackCounter = 0

  SysCmd.acquire
  protected var released = false

  //////////////
  // Plumbing //
  //////////////

  protected val solver = java.lang.Runtime.getRuntime.exec(Array(cmd) ++ options, null, null)
  protected val solverInput = new BufferedWriter(new OutputStreamWriter(solver.getOutputStream()))
  protected val solverOutput = new BufferedReader(new InputStreamReader(solver.getInputStream()))
  protected val solverError = new BufferedReader(new InputStreamReader(solver.getErrorStream()))

  //////////////////
  // Declarations //
  //////////////////

  protected val declaredV = HashSet[Variable]()
  protected val declStack = Stack(Set[Variable]())
  
  protected val declaredS = HashSet[Symbol]()
  protected val symbolStack = Stack(Set[Symbol]())
  
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
  declaredS ++= Theory.fun(th)

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
    toSolver("(exit)")
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
  
  def declare(t: Type) = t match {
    case UnInterpreted(id) => toSolver("(declare-sort " + id + " 0)")
    case other => Logger.logAndThrow("smtlib", Error, "not supported: " + other)
  }

  def typeDecl(t: Type) = {
    val (args, ret) = t match {
      case Function(args, r) => (args, r)
      case other => (Nil, other)
    }
    val argsDecl = args.map(Printer.tpe).mkString("("," ",")")
    argsDecl + " " + Printer.tpe(ret)
  }

  def declare(f: Formula) = f match {
    case v @ Variable(_) =>
      toSolver("(declare-fun " + Printer.asVar(v) + " " + typeDecl(v.tpe) + ")")
    case other => Logger.logAndThrow("smtlib", Error, "not supported: " + other)
  }
  
  //TODO overloading
  def declare(s: Symbol) = s match {
    case UnInterpretedFct(f, t, p) =>
      Logger.assert(p.isEmpty, "smtlib", "declaring sym with params: " + p)
      Logger.assert(t.isDefined, "smtlib", "declaring sym with unkown type: " + f)
      toSolver("(declare-fun " + Printer.asVar(f) + " " + typeDecl(t.get) + ")")
    case i: InterpretedFct =>
      toSolver("(declare-fun " + Printer.asVar(i.symbol) + " " + typeDecl(i.tpe) + ")")
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
    val newSym = pushOnStack(FormulaUtils.collectSymbols(f), symbolStack, declaredS)
    newSym foreach declare
    val newVars = pushOnStack(f.freeVariables, declStack, declaredV)
    newVars foreach declare
  }
  
  def assert(f: Formula) {
    if (implicitDeclaration) {
      mkDeclarations(f)
    }
    //(assert f)
    Logger("smtlib", Debug, Printer(_, f))
    solverInput.write("(assert ")
    Printer(solverInput, f)
    solverInput.write(")")
    solverInput.newLine
    //solverInput.flush
  }
  
  def push {
    if (implicitDeclaration) {
      declStack.push(Set[Variable]())
      symbolStack.push(Set[Symbol]())
      typeStack.push(Set[Type]())
    }
    stackCounter += 1
    toSolver("(push 1)")
  }
  
  def pop {
    if (implicitDeclaration) {
      declaredV --= declStack.pop
      declaredS --= symbolStack.pop
      declaredT --= typeStack.pop
    }
    Logger.assert(stackCounter > 0, "smtlib", "pop -> stackCounter = " + stackCounter)
    toSolver("(pop 1)")
    stackCounter -= 1
  }
  
  def checkSat: Option[Boolean] = {
    toSolver("(check-sat)")
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
