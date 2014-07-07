package round.utils.report

class List(title: String) extends Sequence(title) {
  
  override def toText(writer: java.io.BufferedWriter) = {
    for (c <- children) {
      writer.write("-> ")
      c.toText(writer)
    }
  }

  override def toHtmlInner(writer: java.io.BufferedWriter) = {
    writer.write("<ul>"); writer.newLine
    for (c <- children) {
      writer.write("<li>"); writer.newLine
      c.toHtml(writer)
      writer.write("</li>"); writer.newLine
    }
    writer.write("</ul>"); writer.newLine
  }

}

