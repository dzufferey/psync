package round.utils.report

import round.utils._
import java.io.{BufferedWriter, PrintWriter, OutputStreamWriter, FileOutputStream}

class Report(title: String) extends List(title) {

  def htmlHeader(writer: BufferedWriter) {
    writer.write("<!DOCTYPE HTML>"); writer.newLine
    writer.write("<html>"); writer.newLine
    writer.write("<head>"); writer.newLine
    writer.write("    <meta charset=\"utf-8\">"); writer.newLine
    val escapedTitle = org.apache.commons.lang3.StringEscapeUtils.escapeHtml4(title)
    writer.write("    <title>Analysis report for "+escapedTitle+"</title>"); writer.newLine
    writer.write(Style.CSS); writer.newLine
    writer.write("</head>"); writer.newLine
    writer.write("<body>"); writer.newLine
  }

  def htmlFooter(writer: BufferedWriter) {
    writer.write("</body>"); writer.newLine
    writer.write("</html>"); writer.newLine
  }

  
  def makeConsoleReport {
    val writer = new BufferedWriter(new PrintWriter(Console.out))
    toText(writer)
    writer.flush
  }

  override def toHtml(writer: BufferedWriter) {
    printHtmlTitle(writer)
    toc.toHtml(writer)
    toHtmlInner(writer)
  }

  override def toHtmlInner(writer: BufferedWriter) {
    for (c <- children) {
      c.toHtml(writer)
    }
  }
  
  def makeHtmlReport(fileName: String) = {
    val fileOut = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)))
    val toc = new TOC(this)
    htmlHeader(fileOut)
    toHtml(fileOut)
    htmlFooter(fileOut)
    fileOut.close
  }

}

object Report {

  protected var current: Option[Report] = None

  def reset { current = None }

  def get: Report = {
    if (current.isDefined) {
      current.get
    } else {
      Logger.logAndThrow("report", LogError, "Report.get: empty")
    }
  }

  def set(r: Report) {
    if (current.isDefined) {
      Logger("report", LogWarning, "Report.set: report is already defined, replacing")
    }
    current = Some(r)
  }
  
  def add(element: Item) {
    for (r <- current) r.add(element)
  }

}
