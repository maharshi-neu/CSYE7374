package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.language.postfixOps

class PrimesFunctionalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Prime"

  // NOTE: This takes much too long.
  it should "validate" in {
    //    (Prime("35742549198872617291353508656626642567") validate) shouldBe true
  }

  // NOTE: the following is very slow.
  it should "create Mersenne prime" in {
    //        Prime.createMersennePrime(17) map (_.validate) shouldBe Some(true)
  }
}
