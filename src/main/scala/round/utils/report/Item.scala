package round.utils.report

import round.utils._

abstract class Item(val title: String) {

  protected val created = new java.util.Date()

  protected var toc: TocEntry = null
  def setToc(entry: TocEntry) {
    toc = entry
  }
  protected def getHtmlTitle = {
    if (toc != null) {
      toc.getTitleWithRef
    } else {
      Logger.logAndThrow("Report", LogError, "TOC not specified")
    }
  }
  protected def printHtmlTitle(writer: java.io.BufferedWriter) {
    writer.write(getHtmlTitle); writer.newLine
  }

  def children: Seq[Item] = Nil

  def toText(writer: java.io.BufferedWriter)

  def toHtmlInner(writer: java.io.BufferedWriter)

  def toHtml(writer: java.io.BufferedWriter) {
    printHtmlTitle(writer)
    toHtmlInner(writer)
  }

}

