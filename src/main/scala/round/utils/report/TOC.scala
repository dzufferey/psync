package round.utils.report

class TocEntry(val item: Item, val children: Seq[TocEntry]) {

  def this(item: Item) = this(item, item.children.map(new TocEntry(_)))

  //put the to reference in the child
  item.setToc(this)

  private var pathToRoot: Seq[Int] = Nil
  def setPath(toHere: Seq[Int]) {
    pathToRoot = toHere
    children.zipWithIndex.foreach{ case (c, i) => c.setPath(i +: toHere) }
  }

  lazy val ref = pathToRoot.reverse.mkString("Ref_","_","")
  lazy val number = pathToRoot.reverse.mkString("",".","")
  
  def toText(writer: java.io.BufferedWriter) {
    sys.error("TODO")
  }
  
  def toHtml(writer: java.io.BufferedWriter) {
    writer.write("<li>")
    val escapedTitle = org.apache.commons.lang3.StringEscapeUtils.escapeHtml4(item.title)
    writer.write("<a href=\"#"+ref+"\"> <span>"+number+"</span> <span>"+escapedTitle+"</span> </a>")
    if( !children.isEmpty) {
      writer.newLine
      writer.write("<ul>"); writer.newLine
      for (c <- children ) c.toHtml(writer)
      writer.write("</ul>"); writer.newLine
    }
    writer.write("</li>"); writer.newLine
  }

  def getTitleWithRef = {
    val titleLevel = pathToRoot.length match {
      case 0 => "h1"
      case 1 => "h2"
      case 2 => "h3"
      case 3 => "h4"
      case 4 => "h5"
      case 5 => "h6"
    }
    val escapedTitle = org.apache.commons.lang3.StringEscapeUtils.escapeHtml4(item.title)
    "<a name=\""+ref+"\"> <"+titleLevel+">"+escapedTitle+"</"+titleLevel+"> </a>"
  }

}

//an abstraction for table of content
class TOC(report: Report) {

  val entries = new TocEntry(report)
  entries.setPath(Nil)
  
  def toText(writer: java.io.BufferedWriter) {
    sys.error("TODO")
  }

  def toHtml(writer: java.io.BufferedWriter) {
    writer.write("<div id=\"toc\" class=\"toc\">"); writer.newLine
    writer.write("<div id=\"toctitle\" class=\"toctitle\">Contents</div>"); writer.newLine
    writer.write("<ul>"); writer.newLine
    entries.toHtml(writer)
    writer.write("</ul>"); writer.newLine
    writer.write("</div>"); writer.newLine
  }

}
