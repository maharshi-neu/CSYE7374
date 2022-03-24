package crypto

import scala.util.Random

case class Quiz2(random: Random) {
  val convert: (Double, Double) => Complex = boxMuller
  val randomUniform: LazyList[Double] = LazyList.continually(random.nextDouble())
  val randomPairs: LazyList[(Double, Double)] = randomUniform.grouped(2) map (xs => xs.head -> xs.last) to LazyList
  val randomComplex: LazyList[Complex] = randomPairs map convert.tupled // TODO using randomUniform and boxMuller

  /**
   * Implement the right hand side to evaluate a lazy list of random numbers which are distributed according to Gaussian
   * distribution with mean 0 and std. dev. 1.
   *
   * Use the boxMuller method (as a function) to achieve this.
   */
  def randomGaussian: LazyList[Double] = randomComplex flatMap { c => val x = c.toCartesian; x.toSeq.to(LazyList) } // TODO using randomComplex

  /**
   * Method to invoke the Box-Muller technique to generate an independent pair of Gaussian-distributed random numbers.
   * https://en.wikipedia.org/wiki/Normal_distribution#Generating_values_from_normal_distribution
   *
   * @param u a uniformly distributed double between 0 and 1.
   * @param v a uniformly distributed double between 0 and 1.
   * @return a Polar whose magnitude is -2 ln u and whose phase is 2 pi v.
   */
  def boxMuller(u: Double, v: Double): Complex = Polar(math.sqrt(-2.0 * math.log(u)), 2.0 * math.Pi * v)

}

trait Complex {
  def modulus: Double

  def phase: Double

  def complement: Complex

  def plus(c: Complex): Complex

  def times(c: Complex): Complex

  def toCartesian: Cartesian

  def toPolar: Polar
}

case class Cartesian(r: Double, i: Double) extends Complex {
  def plus(c: Complex): Complex = Cartesian(r + c.toCartesian.r, i + c.toCartesian.i)

  def toPolar: Polar = Polar(math.sqrt(r * r + i * i), math.atan2(i, r))

  def modulus: Double = toPolar.modulus

  def phase: Double = toPolar.phase

  def complement: Complex = Cartesian(r, -i)

  def times(c: Complex): Complex = toPolar.times(c)

  def toCartesian: Cartesian = this

  def toSeq: Seq[Double] = Seq(r, i)

  override def equals(obj: Any): Boolean = obj match {
    case c: Cartesian => Complex.approx(r, c.r) && Complex.approx(i, c.i)
    case _ => false
  }
}

case class Polar(r: Double, theta: Double) extends Complex {
  def modulus: Double = r

  def phase: Double = theta

  def complement: Complex = Polar(r, -theta)

  def plus(c: Complex): Complex = toCartesian.plus(c)

  def times(c: Complex): Complex = Polar(r * c.toPolar.r, theta + c.toPolar.theta)

  def toCartesian: Cartesian = Cartesian(r * math.cos(theta), r * math.sin(theta))

  def toPolar: Polar = this

  override def equals(obj: Any): Boolean = obj match {
    case p: Polar => Complex.approx(r, p.r) && Complex.approx(theta, p.theta)
    case _ => false
  }
}

object Complex {
  def parse(u: String, w: String): Option[Complex] = for (x <- u.toDoubleOption; y <- w.toDoubleOption) yield Cartesian(x, y)

  def approx(a: Double, b: Double): Boolean = math.abs(a - b) < 1E-5
}

