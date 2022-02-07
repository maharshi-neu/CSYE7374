package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.language.postfixOps

class PrimesSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Prime"

  it should "isProbablePrime" in {
    Prime.isProbablePrime(2) shouldBe true
    Prime.isProbablePrime(7) shouldBe true
    Prime.isProbablePrime(8) shouldBe false
    Prime.isProbablePrime(11) shouldBe true
    Prime.isProbablePrime(BigInt("35742549198872617291353508656626642567")) shouldBe true
  }

  it should "isProbableOddPrime" in {
    // It's OK to call this method on 2 (but not any other even number).
    Prime.isProbableOddPrime(2) shouldBe true
    Prime.isProbableOddPrime(7) shouldBe true
    Prime.isProbableOddPrime(11) shouldBe true
    Prime.isProbableOddPrime(7919) shouldBe true
    Prime.isProbableOddPrime(BigInt("35742549198872617291353508656626642567")) shouldBe true
  }

  it should "fermat" in {
    Prime(7).fermat(2) shouldBe 1
    Prime(71).fermat(9) shouldBe 1
  }

  it should "implement Lucas for 7 and 71" in {
    Prime(7).Lucas shouldBe true
    Prime(71).Lucas shouldBe true
  }

  it should "implement Lucas()" in {
    val p = Prime(71)
    val pMinus1: BigInt = p.x - 1
    val factors = Prime.factors(pMinus1)
    p.Lucas(pMinus1, factors)(17) shouldBe false
    p.Lucas(pMinus1, factors)(11) shouldBe true
  }

  it should "validate" in {
    (Prime(2) validate) shouldBe true
    (Prime(4) validate) shouldBe false
    (Prime(7) validate) shouldBe true
    (Prime(120) validate) shouldBe false
    (Prime(7919) validate) shouldBe true
  }

  it should "next" in {
    Prime(7).next shouldBe Prime(11)
    Prime(11).next shouldBe Prime(13)
    Prime(13).next shouldBe Prime(17)
    Prime(17).next shouldBe Prime(19)
    Prime(19).next shouldBe Prime(23)
  }

  it should "create primes from Mersenne numbers" in {
    val xs = for (i <- Seq(2, 3, 5, 7, 13, 17, 19, 31)) yield Prime.isProbablePrime(Prime.mersenneNumber(Prime(i)))
    xs.forall(_ == true) shouldBe true
  }

  it should "create Mersenne prime" in {
    Prime.createMersennePrime(1) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(2) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(3) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(4) map (_.validate) shouldBe None
    Prime.createMersennePrime(5) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(6) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(7) map (_.validate) shouldBe Some(true)
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
    // NOTE: see PrimesFunctionalSpec for the next in the series.
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
    val lessThan1000: Seq[Prime] = Primes.probablePrimes(_.x < 1000)
    lessThan1000.size shouldBe 168
    lessThan1000.last shouldBe Prime(997)
  }

  it should "test MillerRabin" in {
    MillerRabin.millerRabinTester("test", "7919") shouldBe "PRIME"
    MillerRabin.millerRabinTester("test", "516119616549881") shouldBe "PRIME"
    MillerRabin.millerRabinTester("test", "516119616549887") shouldBe "COMPOSITE"
  }
}
