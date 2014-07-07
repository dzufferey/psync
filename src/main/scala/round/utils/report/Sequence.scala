
package round.utils.report

class Sequence(title: String) extends Item(title) {
  
  protected val _children = scala.collection.mutable.Buffer[Item]()

  override def children = _children

  def add(element: Item) {
    _children += element
  }

  def toText(writer: java.io.BufferedWriter) = {
    for (c <- children) {
      c.toText(writer)
    }
  }

  def toHtmlInner(writer: java.io.BufferedWriter) = {
    for (c <- children) {
      c.toHtml(writer)
    }
  }

}
