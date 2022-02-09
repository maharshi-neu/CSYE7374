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

  private val p5: Prime = Prime(5)
  private val p7: Prime = Prime(7)
  private val p11: Prime = Prime(11)
  private val p13: Prime = Prime(13)
  private val p17: Prime = Prime(17)
  private val p23: Prime = Prime(23)

  it should "fermat" in {
    p7.fermat(2) shouldBe 1
    Prime(71).fermat(9) shouldBe 1
  }

  it should "implement Lucas for 7 and 71" in {
    p7.Lucas shouldBe true
    Prime(71).Lucas shouldBe true
  }

  it should "implement Lucas()" in {
    val p = Prime(71)
    val pMinus1: BigInt = p.p - 1
    val factors = Prime.factors(pMinus1)
    p.Lucas(pMinus1, factors)(17) shouldBe false
    p.Lucas(pMinus1, factors)(11) shouldBe true
  }

  it should "implement testPrimitiveRoot" in {
    p5.testPrimitiveRoot(2) shouldBe true
    p7.testPrimitiveRoot(2) shouldBe false
    p7.testPrimitiveRoot(3) shouldBe true
    p11.testPrimitiveRoot(2) shouldBe true
    p13.testPrimitiveRoot(2) shouldBe true
    p13.testPrimitiveRoot(6) shouldBe true
    p13.testPrimitiveRoot(7) shouldBe true
    p13.testPrimitiveRoot(11) shouldBe true
    p13.testPrimitiveRoot(3) shouldBe false
    p17.testPrimitiveRoot(5) shouldBe true
    p17.testPrimitiveRoot(7) shouldBe true
    p23.testPrimitiveRoot(2) shouldBe false
    p23.testPrimitiveRoot(3) shouldBe false
    p23.testPrimitiveRoot(5) shouldBe true
    p23.testPrimitiveRoot(7) shouldBe true
    p23.testPrimitiveRoot(10) shouldBe true
    p23.testPrimitiveRoot(15) shouldBe true
    p23.testPrimitiveRoot(17) shouldBe true
    p23.testPrimitiveRoot(20) shouldBe true
    p23.testPrimitiveRoot(21) shouldBe true
  }

  it should "implement primitiveRoot" in {
    p5.primitiveRoot shouldBe BigInt(2)
    p7.primitiveRoot shouldBe BigInt(3)
    // Why do we never get the other roots (6, 7, 11) here? Oh, duh, because it's less than 20 and so we go in sequence.
    p13.primitiveRoot shouldBe BigInt(2)
    val root23 = p23.primitiveRoot
    println(root23)
    Seq(BigInt(5), BigInt(7), BigInt(10), BigInt(11), BigInt(14), BigInt(15), BigInt(17), BigInt(19), BigInt(20), BigInt(21)) contains root23 shouldBe true
  }

  it should "multiplicativeInverse" in {
    p11.multiplicativeInverse(3) shouldBe 4
    p11.multiplicativeInverse(4) shouldBe 3
    p17.multiplicativeInverse(10) shouldBe 12
    p17.multiplicativeInverse(12) shouldBe 10
    p23.multiplicativeInverse(18) shouldBe 9
    p23.multiplicativeInverse(9) shouldBe 18
  }

  ignore should "multiplicativeInverse2" in {
    val g = 7
    val z = p17.modPow(g, 10)
    z shouldBe 2
    BigInt(10).modInverse(17) shouldBe 12
    val y = BigInt(g).pow(10)
    y shouldBe BigInt(282475249L)
    val q = y / 17
    val r = y - q * 17
    q shouldBe BigInt(16616191L)
    r shouldBe BigInt(2)
    y.mod(17) shouldBe 2
    p17.modPow(z, 12) shouldBe g
  }

  it should "validate" in {
    (Prime(2) validate) shouldBe true
    (Prime(4) validate) shouldBe false
    (p7 validate) shouldBe true
    (Prime(120) validate) shouldBe false
    (Prime(7919) validate) shouldBe true
  }

  it should "next" in {
    p7.next shouldBe p11
    p11.next shouldBe p13
    p13.next shouldBe p17
    val p19 = Prime(19)
    p17.next shouldBe p19
    p19.next shouldBe p23
  }

  it should "create primes from Mersenne numbers" in {
    val xs = for (i <- Seq(2, 3, 5, 7, 13, 17, 19, 31)) yield Prime.isProbablePrime(Prime.mersenneNumber(Prime(i)))
    xs.forall(_ == true) shouldBe true
  }

  it should "create Mersenne prime" in {
    Prime.createMersennePrime(0) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(1) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(2) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(3) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(4) map (_.validate) shouldBe None
    Prime.createMersennePrime(5) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(6) map (_.validate) shouldBe Some(true)
    Prime.createMersennePrime(7) map (_.validate) shouldBe Some(true)
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
    val lessThan1000: Seq[Prime] = Primes.probablePrimes(_.p < 1000)
    lessThan1000.size shouldBe 168
    lessThan1000.last shouldBe Prime(997)
  }

  it should "test MillerRabin" in {
    MillerRabin.millerRabinTester("test", "7919") shouldBe "PRIME"
    MillerRabin.millerRabinTester("test", "516119616549881") shouldBe "PRIME"
    MillerRabin.millerRabinTester("test", "516119616549887") shouldBe "COMPOSITE"
  }
}
