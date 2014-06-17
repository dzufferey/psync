package round.utils.report

import round.utils._

class Table(title: String, headers: => Iterable[String], rows: => Iterable[Iterable[String]]) extends Item(title) {

  var tbl: Array[Array[String]] = null

  protected def mkTbl {
    if (tbl == null) {
      tbl = Array(headers.toArray) ++ rows.map(_.toArray)
      val width = tbl(0).size
      for (i <- 1 until tbl.size) {
        Logger.assert(tbl(i).size == width, "report", "Table rows are not uniform.")
      }
    }
  }

  def toText(writer: java.io.BufferedWriter) {
    mkTbl
    val widths = Array.ofDim[Int](tbl(0).size)
    for (i <- 0 until tbl.size;
         j <- 0 until tbl(i).size) {
       widths(j) = math.max(widths(j), tbl(i)(j).length)
    }
    def printCell(i: Int, j: Int) {
      val content = tbl(i)(j)
      val fill = widths(j) - content.size
      writer.write(content)
      for (_ <- 0 until fill) writer.write(' ')
      writer.write('|')
    }
    for (i <- 0 until tbl(0).size) {
      printCell(0, i)
    }
    writer.newLine
    for (_ <- 0 until widths.reduceLeft(_ + _) + widths.size - 1) {
      writer.write('-')
    }
    writer.write('|')
    writer.newLine
    for (i <- 1 until tbl.size) {
      for (j <- 0 until tbl(i).size) {
        printCell(i, j)
      }
      writer.newLine
    }
  }

  def toHtmlInner(writer: java.io.BufferedWriter) = {
    mkTbl
    writer.write("<table>"); writer.newLine
    writer.write("  <tr>"); writer.newLine
    for (i <- 0 until tbl(0).size) {
      writer.write("    <th>" + tbl(0)(i) + "</th>"); writer.newLine
    }
    writer.write("  </tr>"); writer.newLine
    for (i <- 1 until tbl.size) {
      writer.write("  <tr>"); writer.newLine
      for (j <- 0 until tbl(i).size) {
        writer.write("      <td>" + tbl(i)(j) + "</td>"); writer.newLine
      }
      writer.write("  </tr>"); writer.newLine
    }
    writer.write("</table>"); writer.newLine
  }

}
