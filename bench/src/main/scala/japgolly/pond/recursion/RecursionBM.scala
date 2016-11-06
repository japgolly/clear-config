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

  var s: SampleData.All[MathExpr] = _

  @Setup def setup = {
    s = SampleData.sized(size)
  }

  @Benchmark def cataFix = Recursion.cata(MathExpr.eval)(s.fix)
//  @Benchmark def cataMu = Recursion.cata(MathExpr.eval)(s.mu)
//  @Benchmark def cataNu = Recursion.cata(MathExpr.eval)(s.nu)

  @Benchmark def anaFix = Recursion.ana[Fix, MathExpr, Int](SampleData.factors)(size)
//  @Benchmark def anaMu = Recursion.ana[Mu, MathExpr, Int](SampleData.factors)(size)
//  @Benchmark def anaNu = Recursion.ana[Nu, MathExpr, Int](SampleData.factors)(size)
}

object SampleData {
  val factors: Coalgebra[MathExpr, Int] =
    i => if (i < 2) MathExpr.Num(i) else MathExpr.Add(1, i - 1)

  case class All[F[_]](fix: Fix[F], mu: Mu[F], nu: Nu[F])

  def sized(i: Int) = All[MathExpr](
    Recursion.ana[Fix, MathExpr, Int](factors)(i),
    Recursion.ana[Mu, MathExpr, Int](factors)(i),
    Recursion.ana[Nu, MathExpr, Int](factors)(i)
  )

}