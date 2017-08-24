package japgolly.microlibs.testutil

import scalaz.Equal
import scalaz.syntax.equal._
import scala.io.AnsiColor._

object TestUtil extends TestUtil
trait TestUtil {

  def assertEq[A: Equal](actual: A, expect: A): Unit =
    assertEqO(None, actual, expect)

  def assertEq[A: Equal](name: => String, actual: A, expect: A): Unit =
    assertEqO(Some(name), actual, expect)

  private def lead(s: String) = s"$RED_B$s$RESET "
  private def failureStart(name: Option[String], leadSize: Int): Unit = {
    println()
    name.foreach(n => println(lead(">" * leadSize) + BOLD + YELLOW + n + RESET))
  }

  def assertEqO[A: Equal](name: => Option[String], actual: A, expect: A): Unit =
    if (actual ≠ expect) {
      failureStart(name, 7)

      val toString: Any => String = {
        case s: Stream[_] => s.force.toString() // SI-9266
        case a            => a.toString
      }

      val as = toString(actual)
      val es = toString(expect)
      val ss = as :: es :: Nil
      var pre = "["
      var post = "]"
      val htChars = ss.flatMap(s => s.headOption :: s.lastOption :: Nil)
      if (htChars.forall(_.exists(c => !Character.isWhitespace(c)))) {
        pre = ""
        post = ""
      }
      if (ss.exists(_ contains "\n")) {
        pre = "↙[\n"
      }
      println(lead("expect:") + pre + BOLD + GREEN + es + RESET + post)
      println(lead("actual:") + pre + BOLD + RED + as + RESET + post)
      println()
      fail(s"assertEq${name.fold("")("(" + _ + ")")} failed.")
    }

  def assertMultiline(actual: String, expect: String): Unit =
    if (actual != expect) {
      println()
      val AE = List(actual, expect).map(_.split("\n"))
      val List(as, es) = AE
      val lim = as.length max es.length
      val List(maxA,_) = AE.map(x => (0 #:: x.map(_.length).toStream).max)
      val maxL = lim.toString.length
      println("A|E")
      val fmt = s"%s%${maxL}d: %-${maxA}s |%s| %s$RESET\n"
      def removeWhitespace(s: String) = s.filterNot(_.isWhitespace)
      for (i <- 0 until lim) {
        val List(a, e) = AE.map(s => if (i >= s.length) "" else s(i))
        val ok = a == e
        val cmp = if (ok) " " else if (removeWhitespace(a) == removeWhitespace(e)) "≈" else "≠"
        val col = if (ok) BOLD + BLACK else WHITE
        printf(fmt, col, i + 1, a, cmp, e)
      }
      println()
      fail("assertMultiline failed.")
    }

  def assertMap[K, V: Equal](actual: Map[K, V], expect: Map[K, V]): Unit =
    assertMapO(None, actual, expect)

  def assertMap[K, V: Equal](name: => String, actual: Map[K, V], expect: Map[K, V]): Unit =
    assertMapO(Some(name), actual, expect)

  def assertMapO[K, V: Equal](name: => Option[String], actual: Map[K, V], expect: Map[K, V]): Unit = {
    assertSet(name.fold("Map keys")(_ + " keys"), actual.keySet, expect.keySet)
    val bad = actual.keysIterator.filter(k => actual(k) ≠ expect(k))
    if (bad.nonEmpty) {
      val x = bad.toVector
      for (k <- x) {
        println(s"MapKey: $k")
        println(s"Expect: $BOLD$GREEN${expect(k)}$RESET")
        println(s"Actual: $BOLD$RED${actual(k)}$RESET")
        println()
      }
      fail(s"assertMap${name.fold("")("(" + _ + ")")} failed with ${x.length} value discrepancies.")
    }
  }

  def assertSet[A](actual: Set[A])(expect: A*): Unit = assertSet(actual, expect.toSet)
  def assertSet[A](actual: Set[A], expect: Set[A]): Unit = assertSetO(None, actual, expect)
  def assertSet[A](name: => String, actual: Set[A])(expect: A*): Unit = assertSet(name, actual, expect.toSet)
  def assertSet[A](name: => String, actual: Set[A], expect: Set[A]): Unit = assertSetO(Some(name), actual, expect)

  def assertSetO[A](name: => Option[String], actual: Set[A], expect: Set[A]): Unit =
    if (actual != expect) {
      val missing = expect -- actual
      val unexpected = actual -- expect

      val leadSize = 9
      //if (missing.nonEmpty || unexpected.nonEmpty)
      //fail(s"Actual: $actual\nExpect: $expect\n   Missing: $missing\nUnexpected: $unexpected")
      def show(title: String, col: String, s: Set[A]): Unit =
        if (s.nonEmpty) {
          //val x = if (s.size == 1) s.head.toString else s.mkString("{ ",", "," }")
          val x = s.iterator.map(_.toString).toVector.sorted.mkString("\n" + (" " * (leadSize + 1)))
          println(lead(title) + col + x + RESET)
        }

      failureStart(name, leadSize)
      show(" missing:", BOLD + CYAN, missing)
      show("unwanted:", BOLD + RED, unexpected)
      println()
      fail(s"assertSet${name.fold("")("(" + _ + ")")} failed.")
    }

  def fail(msg: String, clearStackTrace: Boolean = true): Nothing =
    _fail(colourMultiline(msg, BOLD + MAGENTA), clearStackTrace)

  def _fail(msg: String, clearStackTrace: Boolean = true): Nothing = {
    val e = new AssertionError(msg)
    if (clearStackTrace)
      e.setStackTrace(Array.empty)
    throw e
  }

  private def colourMultiline(text: String, colour: String): String =
    colour + text.replace("\n", "\n" + colour) + RESET

  def assertContainsCI(actual: String, expectFrag: String): Unit =
    assertContains(actual.toLowerCase, expectFrag.toLowerCase)

  def assertContains(actual: String, expectFrag: String): Unit =
    if (!actual.contains(expectFrag)) {
      val a = colourMultiline(actual, BOLD + CYAN)
      _fail(s"${BOLD}${MAGENTA}Expected [${GREEN}$expectFrag${MAGENTA}] in:$RESET\n$a")
    }

  def assertChange[A, B: Equal, R](query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B): R =
    assertChangeO(None, query, block)(actual)(expect)

  def assertChange[A, B: Equal, R](desc: => String, query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B): R =
    assertChangeO(Some(desc), query, block)(actual)(expect)

  def assertChangeO[A, B: Equal, R](desc: => Option[String], query: => A, block: => R)(actual: (A, A) => B)(expect: (A, R) => B): R = {
    val before = query
    val result = block
    val after  = query
    assertEqO(desc, actual(after, before), expect(before, result))
    result
  }

  def assertNoChange[B: Equal, A](query: => B)(block: => A): A =
    assertNoChangeO(None, query)(block)

  def assertNoChange[B: Equal, A](desc: => String, query: => B)(block: => A): A =
    assertNoChangeO(Some(desc), query)(block)

  def assertNoChangeO[B: Equal, A](desc: => Option[String], query: => B)(block: => A): A =
    assertChangeO(desc, query, block)((b, _) => b)((b, _) => b)

  def assertDifference[N: Numeric : Equal, A](query: => N)(expect: N)(block: => A): A =
    assertDifferenceO(None, query)(expect)(block)

  def assertDifference[N: Numeric : Equal, A](desc: => String, query: => N)(expect: N)(block: => A): A =
    assertDifferenceO(Some(desc), query)(expect)(block)

  def assertDifferenceO[N: Numeric : Equal, A](desc: => Option[String], query: => N)(expect: N)(block: => A): A =
    assertChangeO(desc, query, block)(implicitly[Numeric[N]].minus)((_, _) => expect)

  def quoteStringForDisplay(s: String): String = {
    val sb = new StringBuilder
    sb append '⟪'
    s foreach {
      case '\b' => sb append '\\'; sb append 'b'
      case '\f' => sb append '\\'; sb append 'f'
      case '\n' => sb append '\\'; sb append 'n'
      case '\r' => sb append '\\'; sb append 'r'
      case '\t' => sb append '\\'; sb append 't'
      case '\\' => sb append '\\'; sb append '\\'
      case c    =>
        if (c >= ' ' && c <= '~')
          sb append c
        else {
          val hex = Integer.toHexString(c.toInt)
          sb append "\\u"
          hex.length match {
            case 1 => sb append "000"
            case 2 => sb append "00"
            case 3 => sb append '0'
            case _ =>
          }
          sb append hex
        }
    }
    sb append '⟫'
    sb.toString()
  }

}
