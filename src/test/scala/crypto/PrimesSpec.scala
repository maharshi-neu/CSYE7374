package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.language.postfixOps

class PrimesSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Prime"

  it should "isProbablePrime" in {
    (Primes.isProbablePrime(7)) shouldBe true
    (Primes.isProbablePrime(7919)) shouldBe true
    (Primes.isProbablePrime(BigInt("35742549198872617291353508656626642567"))) shouldBe true
  }

  it should "validate" in {
    (Prime(2) validate) shouldBe true
    (Prime(4) validate) shouldBe false
    (Prime(7) validate) shouldBe true
    (Prime(120) validate) shouldBe false
    (Prime(7919) validate) shouldBe true
    // This next currently takes too long
//    (Prime("35742549198872617291353508656626642567") validate) shouldBe true
  }

  it should "next" in {
    Prime(7).next shouldBe Prime(11)
    Prime(11).next shouldBe Prime(13)
    Prime(13).next shouldBe Prime(17)
    Prime(17).next shouldBe Prime(19)
    Prime(19).next shouldBe Prime(23)
  }

  it should "get first 100 primes" in {
    val first100: Seq[Prime] = Primes.allPrimes.take(100).toList
    first100.last shouldBe Prime(541)
  }

  it should "get first 1000 primes" in {
    val first1000: Seq[Prime] = Primes.allPrimes.take(1000).toList
    first1000.last shouldBe Prime(7919)
  }

  it should "get primes < 1000" in {
    val lessThan1000: Seq[Prime] = Primes.primes(_.x < 1000).toList
    lessThan1000.size shouldBe 168
    lessThan1000.last shouldBe Prime(997)
  }

  it should "test MillerRabin" in {
    MillerRabin.millerRabinTester("test", "516119616549881") shouldBe "PRIME"
    MillerRabin.millerRabinTester("test", "516119616549887") shouldBe "COMPOSITE"
  }
}
