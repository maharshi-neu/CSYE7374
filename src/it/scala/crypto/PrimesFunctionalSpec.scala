package crypto

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

  // NOTE: the following is very slow.
  it should "create Mersenne prime" in {
    //        Prime.createMersennePrime(17) map (_.validate) shouldBe Some(true)
  }
}
