package japgolly.microlibs.recursion

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
    s = Recursion.ana(MathExpr.plusOnes)(size)
  }

  @Benchmark def cata: Int = Recursion.cata(MathExpr.eval)(s)
  @Benchmark def ana: Fix[MathExpr] = Recursion.ana(MathExpr.plusOnes)(size)
  @Benchmark def hylo: Int = Recursion.hylo(MathExpr.plusOnes, MathExpr.eval)(size)
}
