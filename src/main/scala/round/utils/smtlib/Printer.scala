package round.utils.smtlib

import round.formula._
import round.utils._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.io._
import Names._

object Printer {
  
  //TODO refactor to print Command
  
  def printable(str: String): String = {
    assert(str.length > 0)
    val noDollars = str.replace("$","_")
    if (noDollars startsWith "_") "v" + noDollars
    else noDollars
  }

  protected def asDecl(v: Variable): String = {
    "(" + printable(v.name) + " " + tpe(v.tpe) + ")"
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
    case Variable(v) => writer.write(printable(v))
    case Literal(l: Int) => if (l >= 0) writer.write(l.toString) else writer.write("(- " + (-l).toString + ")")
    case Literal(l) => writer.write(l.toString)
    case app @ Application(fct, args) => 
      val params = FormulaUtils.typeParams(app)
      writer.write("(")
      writer.write(printable(overloadedSymbol(fct, params)))
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
      writer.write(printable(id))
      writer.write(" ")
      writer.write(arity.toString)
      writer.write(")")

    case DeclareFun(id, sig) =>
      writer.write("(declare-fun ")
      writer.write(printable(id))
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
