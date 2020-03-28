package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal(
      b() * b() - 4 * a() * c()
    )

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal(
      if (delta() < 0) Set.empty
      else {
        val root_1 = (-b() + Math.sqrt(delta())) / (2 * a())
        val root_2 = (-b() - Math.sqrt(delta())) / (2 * a())
        Set(root_1, root_2)
      }
    )
}
