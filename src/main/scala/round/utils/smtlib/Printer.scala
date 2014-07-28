package round.utils.smtlib

import round.formula._
import round.utils._
import round.utils.LogLevel._
import java.io._

object Printer {
  
  protected def symbol(i: Symbol): String = i match {
    case Implies => "=>"
    case Or => "or"
    case And => "and"
    case Not => "not"
    case Eq => "="
    case Geq => ">="
    case Leq => "<="
    case Gt => ">"
    case Lt => "<"
    case Plus => "+"
    case Minus => "-"
    case Times => "*"
    case UnInterpretedFct(f, _, _) => f
    case Neq => Logger.logAndThrow("smtlib", Error, "≠ should be replaced by Not(Eq(...))")
    case i: InterpretedFct => i.symbol
  }

  def tpe(t: Type): String = t match {
    case Bool => "Bool"
    case Int => "Int"
    case Wildcard => "_"
    case FSet(elt) => sys.error("TODO FSet")
    case FOption(elt) => sys.error("TODO FOption")
    case Product(elts) => sys.error("TODO Product")
    case Function(args, returns) => args.map(tpe).mkString("(", ") (", ")") + " (" + tpe(returns) + ")"
    case UnInterpreted(id) => id
    case other => Logger.logAndThrow("smtlib", Error, "not supported: " + other)
  }

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

}
