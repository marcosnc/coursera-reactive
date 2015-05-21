package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal({
             val bVal = b()
             (bVal*bVal) - (4*a()*c())
           })
  }

  def divider(a: Double): Double = if (a != 0) 2 * a else 1

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
             val aValDiv = divider(a())
             val bValNeg = - b()
             val dVal = delta()
             val dValSqrt = Math.sqrt(dVal)
             dVal match
             {
               case dAux if dAux < 0 => Set()
               case 0 => Set(bValNeg / aValDiv)
               case _ => Set(
                 (bValNeg + dValSqrt) / aValDiv,
                 (bValNeg - dValSqrt) / aValDiv)
             }
           })
  }
}
