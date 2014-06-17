package round.utils.text

import java.io.BufferedWriter

/* scala.text.Document got deprecated, here is a replacement.
 * the main difference is that the space vs line return are set by the user (not the pretty printer).
 */

case object Empty extends Document
case object Break extends Document
case object Space extends Document
case class  Text(txt: String) extends Document
case class  Nest(indent: Int, doc: Document) extends Document
case class  Cons(hd: Document, tl: Document) extends Document

sealed abstract class Document {
  def ::(hd: Document): Document = Cons(hd, this)
  def ::(hd: String): Document = Cons(Text(hd), this)
  def :::(hd: Document): Document = hd :: Space :: this
  def :::(hd: String): Document = hd :: Space :: this
  def :/:(hd: Document): Document = hd :: Break :: this
  def :/:(hd: String): Document = hd :: Break :: this

  //returns whether it ended with a new line
  def format(writer: BufferedWriter): Boolean = {

    def spaces(n: Int): Unit = n match {
      case 0 => ()
      case 1 => writer write " ";
      case 2 => writer write "  ";
      case 3 => writer write "   ";
      case 4 => writer write "    ";
      case 5 => writer write "     ";
      case 6 => writer write "      ";
      case 7 => writer write "       ";
      case _ =>
        assert(n >= 8)
        writer write "        ";
        spaces(n - 8)
    }

    def fmt(indent: Int, doc: Document, nl: Boolean): Boolean = doc match {
      case Empty => (); false
      case Space => writer write " "; false
      case Break => writer.newLine(); true
      case Cons(d1, d2) => fmt(indent, d2, fmt(indent, d1, nl))
      case Text(t) => if (nl) spaces(indent); writer write t; false
      case Nest(i, d) => fmt(indent + i, d, nl)
    }

    fmt(0, this, true)
  }

}

object Document {
  def empty = Empty
  def space = Space
  def break = Break
  def text(s: String): Document = Text(s)
  def nest(i: Int, d: Document): Document = Nest(i, d)
}

