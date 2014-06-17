package round.utils.report

class PreformattedText(title: String, content: String) extends Item(title) {

  def toText(writer: java.io.BufferedWriter) = sys.error("TODO")

  def toHtmlInner(writer: java.io.BufferedWriter) = {
    writer.write("<pre>")
    val escaped = org.apache.commons.lang3.StringEscapeUtils.escapeHtml4(content)
    writer.write(escaped)
    writer.write("</pre>")
    writer.newLine
  }

}
