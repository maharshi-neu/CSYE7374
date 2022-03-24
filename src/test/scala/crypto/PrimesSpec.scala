package crypto

import crypto.Primes.piApprox
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.language.postfixOps

class PrimesSpec extends AnyFlatSpec with should.Matchers {

  it should "get first 100 primes" in {
    val first100: Seq[Prime] = Primes.allPrimes.take(100).toList
    first100.last shouldBe Prime(541)
  }

  it should "get first 1000 primes" in {
    val first1000: Seq[Prime] = Primes.allPrimes.take(1000).toList
    first1000.last shouldBe Prime(7919)
  }

  it should "get primes < 1000" in {
    val lessThan1000: Seq[Prime] = Primes.probablePrimes(_.p < 1000)
    lessThan1000.size shouldBe 168
    lessThan1000.last shouldBe Prime(997)
  }

  behavior of "piApprox"

  it should "be correct for specific values" in {
    // 5 (2 3 5 7 11)
    piApprox(11) shouldBe 5.0 +- 1
    // 9 (... 13 17 19 23)
    piApprox(23) shouldBe 9.0 +- 2
    // 11 (... 29 31)
    piApprox(31) shouldBe 11.0 +- 2
    // 13 (... 37 41)
    piApprox(41) shouldBe 13.0 +- 2
  }

  it should "be correct for 10^x" in {
    piApprox(100) shouldBe 25.0 +- 3.5
    piApprox(1000) shouldBe 168.0 +- 24
    piApprox(10000) shouldBe 1229.0 +- 144
  }

  behavior of "MillerRabin"

  it should "test MillerRabin" in {
    MillerRabin.millerRabinTester("test", "7919") shouldBe "PRIME"
    MillerRabin.millerRabinTester("test", "516119616549881") shouldBe "PRIME"
    MillerRabin.millerRabinTester("test", "516119616549887") shouldBe "COMPOSITE"
  }
}
