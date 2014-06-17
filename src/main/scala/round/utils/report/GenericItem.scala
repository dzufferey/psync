package round.utils.report

class GenericItem(title: String, txt: => String, html: => String) extends Item(title) {

  def toText(writer: java.io.BufferedWriter) {
    writer.write(txt); writer.newLine
  }

  def toHtmlInner(writer: java.io.BufferedWriter) = {
    writer.write(html); writer.newLine
  }

}
