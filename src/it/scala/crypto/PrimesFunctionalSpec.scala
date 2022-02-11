package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.language.postfixOps

class PrimesFunctionalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Prime"

  it should "validate random primes" in {
    val bits = 40
    val prime = Primes.randomPrime(bits)
    prime.bits shouldBe bits
    println(prime)
    prime.validate shouldBe true
  }

  // NOTE: This takes much too long.
  it should "validate" in {
    //    (Prime("35742549198872617291353508656626642567") validate) shouldBe true
  }

  it should "create Mersenne prime" in {
    //    println(Prime.mersenneNumber(8))
    Prime.createMersennePrime(8) map (_.validate) shouldBe None
    //    println(Prime.mersenneNumber(9))
    Prime.createMersennePrime(9) map (_.validate) shouldBe None
    Prime.createMersennePrime(10) map (_.validate) shouldBe Some(true)
    //    println(Prime.mersenneNumber(11))
    Prime.createMersennePrime(11) map (_.validate) shouldBe None
    //    println(Prime.mersenneNumber(12))
    Prime.createMersennePrime(12) map (_.validate) shouldBe None
    //    println(Prime.mersenneNumber(13))
    Prime.createMersennePrime(13) map (_.validate) shouldBe None
    Prime.createMersennePrime(14) map (_.validate) shouldBe None
    Prime.createMersennePrime(15) map (_.validate) shouldBe None
    Prime.createMersennePrime(16) map (_.validate) shouldBe None
    Prime.createMersennePrime(17) map (_.validate) shouldBe Some(true)
  }
}
