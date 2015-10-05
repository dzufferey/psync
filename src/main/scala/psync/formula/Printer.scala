package psync.formula

import java.io.{BufferedWriter, StringWriter}

abstract class Printer {
  protected def printFormula(f: Formula, priority: Int = 0)(implicit writer: BufferedWriter): Unit
  def apply(f: Formula)(implicit writer: BufferedWriter): Unit
  def conjunctsTbl(fs: Seq[Formula])(implicit writer: BufferedWriter): Unit

  def toString(f: Formula) = {
    val strw = new StringWriter
    val writer = new BufferedWriter(strw)
    apply(f)(writer)
    writer.flush
    strw.toString
  }
  
  def toStringTbl(fs: Seq[Formula]) = {
    val strw = new StringWriter
    val writer = new BufferedWriter(strw)
    conjunctsTbl(fs)(writer)
    writer.flush
    strw.toString
  }
}

object HtmlPrinter extends Printer {

  protected def printFormula(f: Formula, priority: Int = 0)(implicit writer: BufferedWriter): Unit = f match {
    case Literal(l: Int) => writer.write("<mn>" + l + "</mn>")
    case Literal(l: Long) => writer.write("<mn>" + l + "</mn>")
    case Literal(l: Short) => writer.write("<mn>" + l + "</mn>")
    case Literal(l: Byte) => writer.write("<mn>" + l + "</mn>")
    case Literal(l: Boolean) => writer.write("<mi>" + (if(l) "⊤" else "⊥") + "</mi>")
    case Variable(v) => writer.write("<mi>"+v+"</mi>")
    case Application(fct, args) => 
      if (fct.fix == Fix.Prefix) {
        writer.write("<mrow><mi>" + fct + "</mi><mo>&#x2061;</mo><mfenced>")
        args.foreach(printFormula(_))
        writer.write("</mfenced></mrow>")
      } else if (fct.fix == Fix.Infix) {
        writer.write("<mrow>")
        if(fct.priority < priority) writer.write("<mo>(</mo>") 
        val it = args.iterator
        while(it.hasNext){
          printFormula(it.next, fct.priority)
          if (it.hasNext) writer.write("<mo>"+fct+"</mo>")
        }
        if(fct.priority < priority) writer.write("<mo>)</mo>") 
        writer.write("</mrow>")
      } else { //Fix.Suffix
        writer.write("<mrow><mfenced>")
        args.foreach(printFormula(_)) 
        writer.write("</mfenced><mo>"+fct+"</mo></mrow>")
      }
    case Comprehension(vars, f2) =>
      writer.write("<mrow><mo>{</mo>")
      val it = vars.iterator
      while(it.hasNext){
        printFormula(it.next)
        if (it.hasNext) writer.write("<mo>,</mo>")
      }
      writer.write("<mo>.</mo>")
      printFormula(f2)
      writer.write("<mo>}</mo></mrow>")
    case Binding(b @ (Exists|ForAll), vars, f2) =>
      writer.write("<mrow>")
      if(priority > 0) writer.write("<mo>(</mo>") 
      writer.write("<mo>" + (if (b == Exists) "∃" else "∀") + "</mo>")
      val it = vars.iterator
      while(it.hasNext){
        printFormula(it.next)
        if (it.hasNext) writer.write("<mo>,</mo>")
      }
      writer.write("<mo>.</mo>")
      printFormula(f2)
      if(priority > 0) writer.write("<mo>)</mo>") 
      writer.write("</mrow>")
  }

  def apply(f: Formula)(implicit writer: BufferedWriter) = {
    writer.write("<math>")
    printFormula(f)
    writer.write("</math>")
  }
  
  def conjunctsTbl(fs: Seq[Formula])(implicit writer: BufferedWriter) = {
    writer.write("<math><mtable columnalign=\"left\"><mtr><mtd></mtd><mtd>")
    val it = fs.iterator
    while(it.hasNext){
      printFormula(it.next)
      if (it.hasNext) writer.write("</mtd></mtr><mtr><mtd><mo>∧</mo></mtd><mtd>")
    }
    writer.write("</mtd></mtr></mtable></math>")
  }

}

object TextPrinter extends Printer {

  protected def printFormula(f: Formula, priority: Int = 0)(implicit writer: BufferedWriter): Unit = f match {
    case Literal(l) => writer.write(l.toString)
    case Variable(v) => writer.write(v)
    case Application(fct, args) => 
      if (fct.fix == Fix.Prefix) {
        writer.write(fct + "(")
        val it = args.iterator
        while(it.hasNext){
          printFormula(it.next)
          if (it.hasNext) writer.write(", ")
        }
        writer.write(")")
      } else if (fct.fix == Fix.Infix) {
        if(fct.priority < priority) writer.write("(") 
        val it = args.iterator
        while(it.hasNext){
          printFormula(it.next, fct.priority)
          if (it.hasNext) writer.write(" " + fct + " ")
        }
        if(fct.priority < priority) writer.write(")") 
      } else { //Fix.Suffix
        writer.write("(")
        val it = args.iterator
        while(it.hasNext){
          printFormula(it.next)
          if (it.hasNext) writer.write(", ")
        }
        writer.write(")"+fct)
      }
    case Comprehension(vars, f2) =>
      writer.write("{ ")
      val it = vars.iterator
      while(it.hasNext){
        printFormula(it.next)
        if (it.hasNext) writer.write(", ")
      }
      writer.write(". ")
      printFormula(f2)
      writer.write(" }")
    case Binding(b @ (Exists|ForAll), vars, f2) =>
      if(priority > 0) writer.write("( ") 
      writer.write(if (b == Exists) "∃" else "∀")
      val it = vars.iterator
      while(it.hasNext){
        printFormula(it.next)
        if (it.hasNext) writer.write(", ")
      }
      writer.write(". ")
      printFormula(f2)
      if(priority > 0) writer.write(" )") 
  }
  
  def conjunctsTbl(fs: Seq[Formula])(implicit writer: BufferedWriter) = {
    writer.write("  ")
    val it = fs.iterator
    while(it.hasNext){
      printFormula(it.next)
      if (it.hasNext) {
        writer.newLine
        writer.write("∧ ")
      }
    }
  }

  def apply(f: Formula)(implicit writer: BufferedWriter) = {
    printFormula(f)
  }

}
