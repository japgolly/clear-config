package japgolly.pond.recursion

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

@Warmup(iterations = 10)
@Measurement(iterations = 10)
@Fork(1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class RecursionBM {

  @Param(Array("10", "100", "1000", "10000"))
  var size: Int = _

  var s: Fix[MathExpr] = _

  @Setup def setup = {
    s = SampleData.sized(size)
  }

  @Benchmark def cata: Int = Recursion.cata(MathExpr.eval)(s)
  @Benchmark def ana: Fix[MathExpr] = Recursion.ana(SampleData.factors)(size)
  @Benchmark def hylo: Int = Recursion.hylo(SampleData.factors, MathExpr.eval)(size)
}

object SampleData {
  val factors: Coalgebra[MathExpr, Int] =
    i => if (i < 2) MathExpr.Num(i) else MathExpr.Add(1, i - 1)

  def sized(i: Int) = Recursion.ana(factors)(i)
}