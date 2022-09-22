package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.language.postfixOps
import scala.util.Random

class Quiz2Spec extends AnyFlatSpec with should.Matchers {

  def square(x: Double): Double = x * x

  behavior of "Complex"

  it should "convert polar/cartesian" in {
    val c = Cartesian(2, 3)
    val p = c.toPolar
    p.toCartesian shouldBe c
  }

  it should "convert cartesian/polar" in {
    val p = Polar(2, 3)
    val c = p.toCartesian
    c.toPolar shouldBe p
  }

  behavior of "Quiz2"

  it should "generate a list of Gaussian-distributed random numbers" in {
    val n = 100
    val quiz2: Quiz2 = Quiz2(new Random())
    val xs = quiz2.randomGaussian.take(n).to(List)
    val mean = xs.sum / n
    mean shouldBe 0.0 +- 0.3
    val squares = xs map (_ - mean) map square
    val variance = squares.sum / n
    val stdDev = math.sqrt(variance)
    stdDev shouldBe 1.0 +- 0.2
  }
}

