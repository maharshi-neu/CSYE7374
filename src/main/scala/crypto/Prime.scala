package crypto

import crypto.Primes.{allPrimes, carmichael, hundredPrimes}

import java.math.BigInteger
import scala.collection.SortedSet
import scala.util.Random

/**
 * Class to represent a (probable) prime number.
 * The validate method can be invoked to test if it is really prime.
 *
 * NOTE: just because we have an instance of Prime does not mean that it is a prime number.
 *
 * @param x the value of the prime number.
 */
case class Prime(x: BigInt) extends AnyVal with Ordered[Prime] {
  /**
   * Get the length of this prime in bits.
   *
   * @return the length in bits.
   */
  def bits: Int = x.bigInteger.bitLength()

  /**
   * Method to determine if this is indeed a probably prime.
   *
   * NOTE: ideally, we should be able to substitute Lucas here.
   *
   * @return the result of Prime.isProbablePrime(x).
   */
  def isProbablePrime: Boolean = Prime.isProbablePrime(x)

  /**
   * This yields the value of a.pow(p-1) mod p.
   *
   * @param a a BigInt which is not divisible by p.
   * @return a BigInt which, if this is prime, should be 1 for all 0 < a < p.
   */
  def fermat(a: BigInt): BigInt = modPow(a, x - 1)

  /**
   * Method to apply the Lucas test of primality.
   *
   * TODO determine why this is so slow compared with Miller-Rabin.
   *
   * @return true if for any random value a such that 0 < a < x - 1, Lucas(x-1, factors)(a) is true where factors are the prime factors of x - 1.
   */
  def Lucas: Boolean = {
    val xMinus1: BigInt = x - 1
    val factors = Prime.factors(xMinus1)
    val as: Seq[BigInt] =
      if (xMinus1 < 20) Range(2, x.toInt).map(BigInt(_))
      else RandomState.lazyList(System.nanoTime()).map(_.value(xMinus1 - 1) + 2) take 20
    // NOTE that as will contain duplicates. We should try to eliminate the duplicates.
    as.exists(Lucas(xMinus1, factors)(_))
  }

  /**
   * Method to apply the Lucas test of primality.
   *
   * @param c  the value that is one less than a candidate prime number (i.e. a composite number).
   * @param qs the prime factors of xMinus1.
   * @param a  an arbitrary BigInt such that 0 < a < c.
   * @return fermat(a) && doLucas(c, qs)(a)
   */
  def Lucas(c: BigInt, qs: Seq[Prime])(a: BigInt): Boolean = fermat(a) == 1 && doLucas(c, qs)(a)

  /**
   * Return a Boolean which is true if a.pow(xMinus1/q) != 1 mod x for all q where q is a prime factor of xMinus1.
   *
   * @param c  the value that is one less than a candidate prime number (i.e. a composite number).
   * @param qs the prime factors of xMinus1.
   * @param a  an arbitrary BigInt.
   * @return true if for all q, a.pow(c/q) != 1 mod x
   */
  private def doLucas(c: BigInt, qs: Seq[Prime])(a: BigInt): Boolean = qs.forall(q => modPow(a, c / q.x) != 1)

  /**
   * Validate whether this number really is prime.
   *
   * NOTE: This is a very expensive operation as it essentially performs an E-sieve on the given prime.
   *
   * @return true if this number is prime.
   */
  def validate: Boolean = isProbablePrime && Prime.factors(x).isEmpty

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
   * Equivalent to Prime(x.bigInteger.nextProbablePrime())
   *
   * @return a probable prime which is greater than this.
   */
  def next: Prime = {
    def tens(b: BigInt): LazyList[BigInt] = b #:: tens(b + 10)

    // Lazy list of numbers that could conceivably be primes: all numbers ending with 1, 3, 7, or 9.
    val ys = for (y <- tens(x / 10 * 10); i <- Seq(1, 3, 7, 9)) yield y + i
    // Lazy list of possible primes larger than x.
    val xs = ys.dropWhile(_ <= x).dropWhile(!Prime.isProbableOddPrime(_))
    // Return the first probable prime.
    Prime(xs.take(1).head)
  }

  def compare(that: Prime): Int = x.compare(that.x)

  override def toString: String = Prime.formatWithCommas(x)

  private def modPow(a: BigInt, n: BigInt): BigInt = a.bigInteger.modPow(n, x)
}

object Prime {

  /**
   * Method to yield the prime factors of x.
   * NOTE that this method can be quite expensive.
   *
   * @param x a positive BigInt.
   * @return a Seq[Prime]
   */
  def factors(x: BigInt): Seq[Prime] = if (x > 0) {
    val max = math.sqrt(x.toDouble)
    val candidates = Primes.probablePrimes(p => p.toDouble <= max)
    candidates filter (f => x % f.x == 0)
  }
  else throw PrimeException(s"factors: x is not positive: $x")

  val commaFormatter = new java.text.DecimalFormat("#,###")

  def formatWithCommas(x: BigInt): String = commaFormatter.format(x)

  /**
   * Method to construct a new Prime, provided that it is positive.
   *
   * NOTE there is no check at all as to whether the result is actually a prime number.
   *
   * @param x a positive BigInt.
   * @return a Prime whose value may or may not be a Prime number.
   * @throws PrimeException if x is not positive.
   */
  def apply(x: BigInt): Prime = if (x > 0) new Prime(x) else throw PrimeException("prime must be positive")

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
  def create(x: BigInt): Option[Prime] = optionalPrime(Prime(x))

  /**
   * Function to lift a Prime to an Option[Prime] whose values depends on the result of invoking isProbablePrime.
   */
  val optionalPrime: Prime => Option[Prime] = FP.optional[Prime](_.isProbablePrime)

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
   * Method to detect if x has one of the smaller factors.
   * NOTE: we don't test for odd parity because we assume that x is odd.
   *
   * @param x an odd BigInt
   * @return true if 3 or 5 or 7 divides x.
   */
  def hasSmallFactor(x: BigInt): Boolean = (3 |> x) || (5 |> x) || (7 |> x)

  /**
   * Method to determine if x is a probable prime.
   * We use the MillerRabin test on x.
   * NOTE: we assume that x is odd.
   *
   * More or less the equivalent of x.bigInteger.isProbablePrime(100).
   *
   * @param x an odd BigInt.
   * @return true if x is probably prime.
   */
  def isProbableOddPrime(x: BigInt): Boolean =
    hundredPrimes.contains(Prime(x)) || (x <= 7 || !hasSmallFactor(x)) && !carmichael.contains(x) && MillerRabin.isProbablePrime(x)

  /**
   * Method to determine if x is a probable prime.
   * We use the MillerRabin test on x.
   *
   * @param x a BigInt.
   * @return true if x is probably prime.
   */
  def isProbablePrime(x: BigInt): Boolean = (x == 2 || !(2 |> x)) && isProbableOddPrime(x)
}

object Primes {
  /**
   * Random source.
   */
  val random: java.util.Random = new java.util.Random()

  /**
   * The measure of certainty that we use.
   * Probability of a false positive prime is 2.pow(-100).
   */
  val CERTAINTY = 100

  /**
   * Method to generate a random prime of size bits bits.
   *
   * @param bits the size of the resulting Prime in bits.
   * @return a Prime number with certainly 2.pow(-100).
   */
  def randomPrime(bits: Int): Prime = Prime(new BigInteger(bits, CERTAINTY, random))

  /**
   * Method to yield a list of probable primes as long as they satisfy the predicate f.
   *
   * @param f the predicate to be applied to each candidate prime.
   * @return a List[Prime] where each element satisfies the predicate f.
   * @throws PrimeException if f does not yield finite list.
   */
  def probablePrimes(f: Prime => Boolean): List[Prime] = {
    val result = probablePrimesLazy(f)
    if (result.knownSize == -1) result.toList
    else throw PrimeException("probablyPrimes: filter does not yield finite list")
  }

  /**
   * Method to yield a lazy list of all probable primes.
   *
   * @return a LazyList[Prime].
   */
  lazy val allPrimes: LazyList[Prime] = probablePrimesLazy(_ => true)

  /**
   * The first Carmichael numbers, i.e numbers which satisfy Fermat's little theorem but are composite.
   */
  val carmichael: SortedSet[BigInt] =
    SortedSet(561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841, 29341, 41041, 46657, 52633, 62745, 63973, 75361, 101101, 115921, 126217, 162401, 172081, 188461, 252601, 278545, 294409, 314821, 334153, 340561, 399001, 410041, 449065, 488881, 512461).map(BigInt(_))

  /**
   * The first 100 true primes.
   */
  val hundredPrimes: SortedSet[Prime] =
    SortedSet(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
      283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541).map(Prime(_))

  private val prime101 = 547

  /**
   * Method to yield a lazy list of probable primes as long as they satisfy the predicate f.
   * As soon as f returns false, the lazy list terminates.
   *
   * @param f the predicate to be applied to each candidate prime.
   * @return a LazyList[Prime] where each element satisfies the predicate f.
   */
  private def probablePrimesLazy(f: Prime => Boolean): LazyList[Prime] = {
    def inner(p: Prime): LazyList[Prime] = if (f(p)) p #:: inner(p.next) else LazyList.empty

    hundredPrimes.to(LazyList).filter(f) ++ inner(Prime(prime101))
  }
}

/**
 * This code is from link shown below.
 *
 * It's not idiomatic Scala but I will clean it up as we go forward.
 */
object MillerRabin {
  // This code is from link shown below: 'https://www.literateprograms.org/miller-rabin_primality_test__scala_.html'
  def miller_rabin_pass(a: BigInt, n: BigInt): Boolean = {
    val (d, s) = decompose(n)
    var a_to_power: BigInt = a.modPow(d, n)
    if (a_to_power == 1) return true
    else for (_ <- 1 to s) {
      if (a_to_power == n - 1) return true
      else a_to_power = (a_to_power * a_to_power) % n
    }
    a_to_power == n - 1
  }

  /**
   * Method (originally called miller_rabin) from literateprograms with slight improvements for elegance.
   *
   * NOTE: this is equivalent (I believe) to n.bigInteger.isProbablePrime(40),
   * i.e. there's a one-in-a-trillion chance that we get false positive.
   *
   * @param n a BigInt.
   * @return true if n is a probable prime with certainty approximately 2.pow(-40)
   */
  def isProbablePrime(n: BigInt): Boolean = {
    val k = 20
    for (_ <- 1 to k) {
      val rand = new Random()
      var a: BigInt = 0
      while (a == 0) {
        a = BigInt("" + (rand.nextDouble * n.doubleValue).toInt)
      }
      if (!miller_rabin_pass(a, n)) return false
    }
    true
  }

  def millerRabinTester(action: String, number: String): String =
    if (action == "test")
      if (isProbablePrime(new BigInt(new BigInteger(number)))) "PRIME"
      else "COMPOSITE"
    else if (action == "genprime") {
      var nbits: BigInt = 0
      var p: BigInt = 0
      nbits = new BigInt(new BigInteger(number))
      var rand: java.util.Random = new java.util.Random(System.currentTimeMillis())
      p = new BigInt(new BigInteger(nbits.intValue, rand))
      while (!isProbablePrime(p) || (2 |> p) || (3 |> p) || (5 |> p) || (7 |> p)) {
        rand = new java.util.Random(System.currentTimeMillis())
        p = new BigInt(new BigInteger(nbits.intValue, rand))
      }
      p.toString()
    }
    else s"invalid action: $action"

  private def decompose(n: BigInt) = {
    var d: BigInt = n - 1
    var s: Int = 0
    while (2 |> d) {
      d >>= 1
      s += 1
    }
    (d, s)
  }
}

case class PrimeException(str: String) extends Exception(str)