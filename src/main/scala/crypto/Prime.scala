package crypto

import crypto.Primes.{allPrimes, standardPrimes}

import java.math.BigInteger
import scala.util.Random

/**
 * Class to represent a (probable) prime number.
 * The validate method can be invoked to test if it is really prime.
 *
 * NOTE: just because we have an instance of Prime does not mean that it is a prime number.
 *
 * @param x the value of the prime number.
 */
case class Prime(x: BigInt) extends AnyVal {
  /**
   * Validate whether this number really is prime.
   *
   * @return true if this number is prime.
   */
  def validate: Boolean = standardPrimes.contains(this) || Prime.isProbablePrime(x) && {
    val max = math.sqrt(x.toDouble)
    val candidates: List[Prime] = Primes.probablePrimes(p => p.toDouble <= max).toList
    val factors: List[Prime] = candidates filter (f => x % f.x == 0)
    factors.isEmpty
  }

  /**
   * Get the remainder from the division y/x.
   *
   * @param y a BigInt.
   * @return y % x
   */
  def remainder(y: BigInt): BigInt = y % x

  /**
   * Get the value of this prime.
   *
   * @return x.
   */
  def toBigInt: BigInt = x

  /**
   * Get the value of this prime at a Double.
   *
   * @return x.toDouble
   */
  def toDouble: Double = x.toDouble

  /**
   * Optionally get the value of this prime as a Long.
   *
   * @return Some(x) if it fits as a Long, otherwise None.
   */
  def toLong: Option[Long] = if (x.abs < Long.MaxValue) Some(x.toLong) else None

  /**
   * Optionally get the value of this prime as an Int.
   *
   * @return Some(x) if it fits as an Int, otherwise None.
   */
  def toInt: Option[Int] = if (x.abs < Int.MaxValue) Some(x.toInt) else None

  /**
   * Get the next probable prime number after this one.
   *
   * @return a probable prime which is greater than this.
   */
  def next: Prime = {
    def tens(b: BigInt): LazyList[BigInt] = b #:: tens(b + 10)

    // Lazy list of numbers that could conceivably be primes: all numbers ending with 1, 3, 7, or 9.
    val ys = for (y <- tens(x / 10 * 10); i <- Seq(1, 3, 7, 9)) yield y + i
    // Lazy list of possible primes larger than x.
    val xs = ys.dropWhile(_ <= x).dropWhile(!Prime.isProbablePrime(_))
    // Return the first probable prime.
    Prime(xs.take(1).head)
  }
}

object Prime {
  /**
   * Method to create a (probable) Prime from a String.
   *
   * @param x the String.
   * @return an optional Prime whose value is the String.
   */
  def create(x: String): Option[Prime] = create(BigInt(x))

  /**
   * Create an (optional) instance of Prime such that x is a probable prime.
   *
   * @param x a BigInt.
   * @return an Option[Prime]
   */
  def create(x: BigInt): Option[Prime] = if (isProbablePrime(x)) Some(Prime(x)) else None

  /**
   * Create an (optional) Mersenne Prime of form (2 to the power of the ith prime) - 1.
   *
   * @param i the index of the exponent of two.
   * @return an Option[Prime]
   */
  def createMersennePrime(i: Int): Option[Prime] = create(mersenneNumber(i))

  /**
   * Method to yield a Mersenne number: (2 to the power of the ith prime) - 1.
   *
   * @param i the index of the exponent of two.
   * @return a BigInt.
   */
  def mersenneNumber(i: Int): BigInt = mersenneNumber(allPrimes(i))

  /**
   * Method to yield a Mersenne number: (2 to the power of x) - 1.
   *
   * @param p the prime number to generate the Mersenne prime.
   *          NOTE that no explicit check is made to ensure that p is prime.
   * @return a BigInt.
   */
  def mersenneNumber(p: Prime): BigInt = BigInt(2).pow(p.x.toInt) - 1

  /**
   * Method to determine if x is a probable prime.
   * We use the MillerRabin test on x.
   *
   * @param x a BigInt.
   * @return true if x is probably prime.
   */
  def isProbablePrime(x: BigInt): Boolean = MillerRabin.miller_rabin(x)

}

object Primes {
  /**
   * Method to yield a lazy list of probable primes as long as they satisfy the predicate f.
   * As soon as f returns false, the lazy list terminates.
   *
   * @param f the predicate to be applied to each candidate prime.
   * @return a LazyList[Prime] where each element satisfies the predicate f.
   */
  def probablePrimes(f: Prime => Boolean): LazyList[Prime] = {
    def inner(p: Prime): LazyList[Prime] = if (f(p)) p #:: inner(p.next) else LazyList.empty

    standardPrimes.to(LazyList).filter(f) ++ inner(Prime(271))
  }

  /**
   * Method to yield a lazy list of all probable primes.
   *
   * @return a LazyList[Prime].
   */
  lazy val allPrimes: LazyList[Prime] = probablePrimes(_ => true)

  /**
   * The first true primes.
   */
  val standardPrimes: Seq[Prime] =
    Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269).map(Prime(_))
}

/**
 * This code is from 'https://www.literateprograms.org/miller-rabin_primality_test__scala_.html'
 *
 * It's not idiomatic Scala but I will clean it up as we go forward.
 */
object MillerRabin {
  def miller_rabin_pass(a: BigInt, n: BigInt): Boolean = {
    var d: BigInt = 0
    var s: BigInt = 0
    var a_to_power: BigInt = 0
    d = n - 1
    s = 0
    while (d % 2 == 0) {
      d >>= 1
      s += 1
    }
    a_to_power = a.modPow(d, n)
    if (a_to_power == 1) {
      return true
    }
    for (_ <- 1 to s.intValue) {
      if (a_to_power == n - 1) {
        return true
      }
      a_to_power = (a_to_power * a_to_power) % n
    }
    a_to_power == n - 1
  }

  def miller_rabin(n: BigInt): Boolean = {
    val k: Int = 20
    for (_: Int <- 1 to k) {
      var a: BigInt = 0
      val rand: scala.util.Random = new Random(new java.util.Random())
      while (a == 0) {
        a = new BigInt(new BigInteger("" + (rand.nextDouble * n.doubleValue).toInt))
      }
      if (!miller_rabin_pass(a, n)) {
        return false
      }
    }
    true
  }

  def millerRabinTester(action: String, number: String): String = {
    if (action == "test") {
      if (miller_rabin(new BigInt(new BigInteger(number))))
        "PRIME"
      else
        "COMPOSITE"
    }
    else if (action == "genprime") {
      var nbits: BigInt = 0
      var p: BigInt = 0
      nbits = new BigInt(new BigInteger(number))
      var rand: java.util.Random = new java.util.Random(System.currentTimeMillis())
      p = new BigInt(new BigInteger(nbits.intValue, rand))
      while (!miller_rabin(p) || p % 2 == 0 || p % 3 == 0 || p % 5 == 0 || p % 7 == 0) {
        rand = new java.util.Random(System.currentTimeMillis())
        p = new BigInt(new BigInteger(nbits.intValue, rand))
      }
      p.toString()
    }
    else
      s"invalid action: $action"
  }
}