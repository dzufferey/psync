package round.utils.smtlib

import round.formula._
import round.utils._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.io._
import Names._

object Printer {
  
  //TODO refactor to print Command
  
  def asVar(str: String): String = {
    assert(str.length > 0)
    val noDollars = str.replace("$","_")
    if (noDollars startsWith "_") "v" + noDollars
    else noDollars
  }
  def asVar(v: Variable): String = asVar(v.name)

  protected def asDecl(v: Variable): String = {
    "(" + asVar(v) + " " + tpe(v.tpe) + ")"
  }
  
  protected def printQuantifier(q: String, vars: Iterable[Variable], f: Formula)(implicit writer: BufferedWriter) {
    writer.write("(")
    writer.write(q)
    writer.write(vars.map(asDecl).mkString(" (", " ", ") "))
    printFormula(f)
    writer.write(")")
  }

  protected def printFormula(f: Formula)(implicit writer: BufferedWriter): Unit = f match {
    case Exists(vars, f2) => printQuantifier("exists", vars, f2)
    case ForAll(vars, f2) => printQuantifier("forall", vars, f2)
    case v @ Variable(_) => writer.write(asVar(v))
    case Literal(l: Int) => if (l >= 0) writer.write(l.toString) else writer.write("(- " + (-l).toString + ")")
    case Literal(l) => writer.write(l.toString)
    case Application(fct, args) => 
      writer.write("(")
      writer.write(symbol(fct))
      for (a <- args) {
        writer.write(" ")
        printFormula(a)
      }
      writer.write(")")
  }

  def apply(implicit writer: BufferedWriter, f: Formula) {
    printFormula(FormulaUtils.flatten(f))
    //writer.newLine
  }
  
  def apply(implicit writer: BufferedWriter, cmd: Command) = cmd match {
    case Assert(f) =>
      writer.write("(assert ")
      printFormula(FormulaUtils.flatten(f))
      writer.write(")")

    case DeclareSort(id, arity) =>
      writer.write("(declare-sort ")
      writer.write(id)
      writer.write(" ")
      writer.write(arity.toString)
      writer.write(")")

    case DeclareFun(id, sig) =>
      writer.write("(declare-fun ")
      writer.write(id)
      writer.write(" ")
      writer.write(typeDecl(sig))
      writer.write(")")

    case DefineSort(id, args, ret) =>
      writer.write("(define-sort ")
      sys.error("TODO define-sort")

    case DefineFun(id, args, ret, body) =>
      writer.write("(define-fun ")
      sys.error("TODO define-fun")

    case Exit => writer.write("(exit)")
    case CheckSat => writer.write("(check-sat)")
    case GetModel => writer.write("(get-model)")
    case Push => writer.write("(push 1)")
    case Pop => writer.write("(pop 1)")
  }

}
