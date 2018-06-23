package japgolly.microlibs.utils

import scala.annotation.tailrec
import japgolly.microlibs.stdlib_ext.StdlibExt._

/**
 * Taken from
 * https://github.com/dsl-platform/101-dsl-examples/blob/master/001-periodic-table-of-elements/scala/src/main/scala/com/dslplatform/examples/AsciiTable.scala
 */
object AsciiTable {
  // Characters that make up the table.
  private val CrossBorder  = '+'
  private val RowBorder    = '-'
  private val ColumnBorder = '|'
  private val HeaderBorder = '-'
  private val NL           = System.getProperty("line.separator")

  case class Cell(row: Int, col: Int)


  /** An entry (a row) is a list of strings (representing columns). A table is
   *  a list of rows.
   */
  def apply(table: Seq[Seq[String]],
            centre: Cell => Boolean = _ => false,
            separateDataRows: Boolean = false): String = {

    val rowCount = table.size
    val maxColLengths = table.transpose.map(_.map(s => if (s eq null) 4 else s.removeAnsiEscapeCodes.length).max)
    val sb = new StringBuilder

    @tailrec
    def doMake(rowNum: Int): Unit =
      if (rowNum == rowCount)
        addSeparator(true)
      else {
        if (rowNum <= 1)
          addSeparator(true)
        else if (separateDataRows)
          addSeparator(false)
        addRow(rowNum)
        doMake(rowNum + 1)
      }

    def addSeparator(isHeader: Boolean): Unit = {
      val border = if (isHeader) HeaderBorder else RowBorder
      sb.append(CrossBorder)
      maxColLengths foreach { length =>
        sb.append(border.toString * (length + 2))
        sb.append(CrossBorder)
      }
      sb.append(NL)
      ()
    }

    def addRow(rowNo: Int): Unit = {
      val row = table(rowNo)
      sb.append(ColumnBorder)
      for (column <- maxColLengths.indices) {
        val c = Cell(rowNo, column)
        var cell = row(column)
        if (cell eq null)
          cell = "null"
        val ws = maxColLengths(column) - cell.removeAnsiEscapeCodes.length + 2
        if (centre(c)) {
          sb.append(" " * (ws/2))
          sb.append(cell)
          sb.append(" " * (ws/2 + ws%2))
        } else {
          sb.append(' ')
          sb.append(cell)
          sb.append(" " * (ws - 1))
        }
        sb.append(ColumnBorder)
      }
      sb.append(NL)
      ()
    }

    doMake(0)
    sb.dropRight(NL.length).toString()
  }
}