package round.utils.report

class List(title: String) extends Item(title) {
  
  protected val _children = scala.collection.mutable.Buffer[Item]()

  override def children = _children

  def add(element: Item) {
    _children += element
  }

  def toText(writer: java.io.BufferedWriter) = {
    for (c <- children) {
      writer.write("-> ")
      c.toText(writer)
    }
  }

  def toHtmlInner(writer: java.io.BufferedWriter) = {
    writer.write("<ul>"); writer.newLine
    for (c <- children) {
      writer.write("<li>"); writer.newLine
      c.toHtml(writer)
      writer.write("</li>"); writer.newLine
    }
    writer.write("</ul>"); writer.newLine
  }

}

