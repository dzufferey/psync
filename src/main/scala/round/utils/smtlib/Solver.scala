package round.utils.smtlib

import round.formula._
import round.utils._
import scala.sys.process._
import java.io._

sealed abstract class Theory
case object QF_LIA extends Theory
case object LIA extends Theory
case object QF_LRA extends Theory
case object LRA extends Theory

class Solver(th: Theory, cmd: String, options: Iterable[String], implicitDeclaration: Boolean = true) {

  //TODO implicit symbol declaration, keep a local stack+set of symbol to know what is declared
  //TODO SMTLIB does not support overloading

  protected var stackCounter = 0

  SysCmd.acquire
  protected var released = false

  protected val solver = java.lang.Runtime.getRuntime.exec(Array(cmd) ++ options, null, null)
  protected val solverInput = new BufferedWriter(new OutputStreamWriter(solver.getOutputStream()))
  protected val solverOutput = new BufferedReader(new InputStreamReader(solver.getInputStream()))
  protected val solverError = new BufferedReader(new InputStreamReader(solver.getErrorStream()))

  protected val declared = scala.collection.mutable.HashSet[Variable]()
  protected val declStack = scala.collection.mutable.Stack(Set[Variable]())

  //initialisation
  Logger("smtlib", LogDebug, "starting: " + (Array(cmd) ++ options).mkString(" "))
  toSolver("(set-option :print-success false)")
  toSolver("(set-logic "+th+")")

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
    Logger("smtlib", LogDebug, "> " +cmd)
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
      Logger.logAndThrow("smtlib", LogError, "solver returned:\n" + acc)
    } else {
      val res = solverOutput.readLine
      Logger("smtlib", LogDebug, "< " + res)
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
    case other => Logger.logAndThrow("smtlib", LogError, "not supported: " + other)
  }

  def declare(f: Formula) = f match {
    case v @ Variable(_) =>
      val (args, ret) = v.tpe match {
        case Function(args, r) => (args, r)
        case other => (Nil, other)
      }
      val argsDecl = args.map(Printer.tpe).mkString("("," ",")")
      toSolver("(declare-fun " + Printer.asVar(v) + " " + argsDecl + " " + Printer.tpe(ret) + ")")
    case other => Logger.logAndThrow("smtlib", LogError, "not supported: " + other)
  }
  
  def assert(f: Formula) {
    if (implicitDeclaration) {
      val newVars = f.freeVariables -- declared
      declared ++= newVars
      val stackFrame = declStack.pop
      declStack.push(stackFrame ++ newVars)
      newVars foreach declare
    }
    //(assert f)
    Logger("smtlib", LogDebug, Printer(_, f))
    solverInput.write("(assert ")
    Printer(solverInput, f)
    solverInput.write(")")
    solverInput.newLine
    //solverInput.flush
  }
  
  def push {
    if (implicitDeclaration) {
      declStack.push(Set[Variable]())
    }
    stackCounter += 1
    toSolver("(push 1)")
  }
  
  def pop {
    if (implicitDeclaration) {
      declared -- declStack.pop
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
      case other => Logger.logAndThrow("smtlib", LogError, "checkSat: solver said " + other)
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
