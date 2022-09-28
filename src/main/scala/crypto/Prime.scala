package crypto

import crypto.Prime.{coprime, primeFactorMultiplicity, totient}
import crypto.Primes.*
import java.math.BigInteger
import java.util.function.Consumer
import scala.annotation.{tailrec, targetName, unused}
import scala.collection.SortedSet
import scala.util.{Failure, Random, Success, Try}
import util.Benchmark

/**
 * Class to represent a (possible) prime number.
 * The isProbablePrime method can be invoked to test if this is probably prime (to a very high certainty).
 * The validate method can be invoked to test if it is definitely prime (but it's expensive!)
 *
 * NOTE: just because we have an instance of Prime does not mean that it is a prime number.
 *
 * @param n the value of the (possible) prime number (should be greater than zero, although this is not checked)
 */
case class Prime(n: BigInt) extends AnyVal with Ordered[Prime] {

  /**
   * Method to evaluate Euler's Totient function for this Prime raised to power r.
   *
   * @param r the exponent (power) by which the prime is raised.
   * @return a BigInt, which represents the number of elements less than n which are relatively prime to n.
   *
   */
  def totient(r: Int): BigInt = n.pow(r - 1) * (n - 1)

  /**
   * Method to multiply this Prime by another to yield a BigInt.
   *
   * @param o the other prime.
   * @return a BigInt whose value is the product of the value of this and the value of o.
   */
  @targetName("times")
  def *(o: Prime): BigInt = n * o.n

  /**
   * Get the length of this prime in bits.
   *
   * @return the length in bits.
   */
  def bits: Int = n.bitLength

  /**
   * Determine the multiplicative inverse of a, modulo n.
   *
   * NOTE: when this is a prime number, the multiplicativeInverse is modPow(a, n - 2); for non-prime moduli, we use the modInverse method on BigInt.
   *
   * @param a the value whose multiplicative inverse we require.
   * @return a number z such that z a is congruent to 1 modulo n.
   */
  def multiplicativeInverse(a: BigInt): BigInt = if isProbablePrime then modPow(a, n - 2) else Prime.multiplicativeInverse(a, n)

  /**
   * Method to determine if this is indeed a probably prime.
   *
   * NOTE: ideally, we should be able to substitute Lucas here.
   *
   * @return the result of Prime.isProbablePrime(n).
   */
  def isProbablePrime: Boolean = Prime.isProbablePrime(n)

  /**
   * This yields the value of a.pow(n-1) mod n.
   *
   * @param a a BigInt which is not divisible by n.
   * @return a BigInt which, if this is prime, should be 1 for all 0 < a < n.
   */
  def fermat(a: BigInt): BigInt = modPow(a, n - 1)

  /**
   * Method to apply the Lucas test of primality.
   *
   * TODO determine why this is so slow compared with Miller-Rabin.
   *
   * @return true if for any random value a such that 0 < a < n - 1, Lucas(n-1, factors)(a) is true where factors are the prime factors of n - 1.
   */
  def Lucas: Boolean =
    val factors = Prime.primeFactors(n - 1)
    val as: Seq[BigInt] = Prime.getRandomValues(n)
    // NOTE that as will contain duplicates. We should try to eliminate the duplicates.
    as.exists(Lucas(n - 1, factors)(_))

  /**
   * Method to apply the Lucas test of primality.
   *
   * @param c  a composite number (typically one less than this prime).
   * @param qs the prime factors of c.
   * @param a  an arbitrary BigInt such that 0 < a < c.
   * @return fermat(a) && doLucas(c, qs)(a)
   */
  def Lucas(c: BigInt, qs: Seq[Prime])(a: BigInt): Boolean = fermat(a) == 1 && doLucas(c, qs)(a)

  /**
   * NOTE: This will yield only one of the primitive roots, the one where k = 1.
   *
   * https://homepages.math.uic.edu/~leon/mcs425-s08/handouts/PrimitiveElements.pdf
   *
   * @return
   */
  def primitiveRoot: BigInt =
    val as: Seq[BigInt] = Prime.getRandomValues(n)
    as.find(a => fermat(a) == 1 && testPrimitiveRoot(a)) match {
      case Some(a) => a
      case None => throw PrimeException(s"primitiveRoot: failed to find primitive root for $this (is it prime?)")
    }

  /**
   * Method to test whether a BigInt is a primitive root of this Prime.
   *
   * @param a a BigInt: the candidate primitive root.
   * @return true if a is a primitive root of this.
   */
  def testPrimitiveRoot(a: BigInt): Boolean = toIntOption match {
    case Some(p) => (2 until p - 1).forall(j => modPow(a, j) != 1)
    case None => throw PrimeException(s"testPrimitiveRoot: this prime is too big")
  }

  /**
   * Validate whether this number really is prime.
   *
   * NOTE: This is a very expensive operation as it essentially performs an E-sieve on the given prime.
   *
   * @return true if this number is prime.
   */
  @unused
  def validate: Boolean = isProbablePrime && Prime.primeFactors(n).forall(_ == this)

  /**
   * Get the remainder from the division x/n.
   *
   * @param x a BigInt.
   * @return x % n
   */
  @unused
  def remainder(x: BigInt): BigInt = x % n

  /**
   * Get the value of this prime.
   *
   * @return n.
   */
  def toBigInt: BigInt = n

  /**
   * Get the value of this prime at a Double.
   *
   * @return n.toDouble
   */
  @unused
  def toDouble: Double = n.toDouble

  /**
   * Optionally get the value of this prime as a Long.
   *
   * @return Some(n) if it fits as a Long, otherwise None.
   */
  @unused
  def toLongOption: Option[Long] = if n.abs < Long.MaxValue then Some(n.toLong) else None

  /**
   * Optionally get the value of this prime as an Int.
   *
   * @return Some(n) if it fits as an Int, otherwise None.
   */
  def toIntOption: Option[Int] = if n.abs < Int.MaxValue then Some(n.toInt) else None

  /**
   * Get the next probable prime number after this one.
   *
   * Equivalent to Prime(n.bigInteger.nextProbablePrime())
   *
   * @return a probable prime which is greater than this.
   */
  def next: Prime =
    // Lazy list of numbers greater than n, that could conceivably be primes: viz. 2, 5, and all numbers ending with 1, 3, 7, or 9.
    val ys: LazyList[BigInt] = bigInts(n + 1).filter { x =>
      x == 2 || x == 5 || {
        val r = x % 10;
        r == 1 || r == 3 || r == 7 || r == 9
      }
    }

    // Lazy list of probable primes larger than n.
    val xs = ys.filter(_.isProbablePrime(40))

    // Return the first probable prime.
    Prime(xs.head)

  /**
   * Method to compare this Prime with that Prime.
   *
   * @param that the comparand.
   * @return -1, 0, or 1 according as this is less than, equal to, or greater than that.
   */
  def compare(that: Prime): Int = n.compare(that.n)

  override def toString: String = Prime.formatWithCommas(n)

  /**
   * Method to yield base raised to the power exponent modulo n (i.e., this prime number).
   *
   * @param base     a BigInt representing the base number.
   * @param exponent a BigInt representing the power to which a will be raised.
   * @return the value of base.pow(exponent) mod n.
   */
  def modPow(base: BigInt, exponent: BigInt): BigInt = base.modPow(exponent, n)

  /**
   * Method to determine if this Prime is coPrime to n.
   * Recall that this class doesn't only have true prime members.
   *
   * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
   *
   * @param x a BigInt.
   * @return true or false.
   */
  def isCoprimeTo(x: BigInt): Boolean = coprime(x, n)

  /**
   * Return a Boolean which is true if a.pow(c/q) != 1 mod n for all q where q is a prime factor of c.
   *
   * CONSIDER renaming this to something more descriptive.
   *
   * @param c  the value that is one less than a candidate prime number (i.e. a composite number).
   * @param qs the prime factors of c.
   * @param a  an arbitrary BigInt.
   * @return true if for all q, a.pow(c/q) != 1 mod n
   */
  private def doLucas(c: BigInt, qs: Seq[Prime])(a: BigInt): Boolean = qs.forall(q => {
    val compositeFactor = c / q.n
    modPow(a, compositeFactor) != 1
  })
}

object Prime {

  /**
   * Method to determine if two BigInts are coprime, i.e. relatively prime.
   *
   * @param x a BigInt.
   * @param p another BigInt.
   * @return true if the gcd of x and p is 1.
   */
  def coprime(x: BigInt, p: BigInt) = p.gcd(x) == 1

  /**
   * Method to yield the least common multiple of two numbers.
   *
   * @param x a BigInt.
   * @param y a BigInt.
   * @return a BigInt that is the least common multiple of x and y.
   */
  def lcm(x: BigInt, y: BigInt): BigInt = x * y / x.gcd(y)

  /**
   * Method to yield the value of the Euler's Totient function (phi) for this Prime.
   *
   * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
   *
   * @param x a BigInt for which we require Euler's totient function.
   * @return a BigInt, which represents the number of elements less than n which are relatively prime to x.
   */
  def totient(x: BigInt): BigInt = primeFactorMultiplicity(x).foldLeft(BigInt(1)) { (phi, factor) => phi * totient(factor) }

  /**
   * Euler's totient function for a prime power.
   *
   * @param factor a tuple of Prime and exponent.
   * @return Euler's totient function for this Prime power.
   */
  def totient(factor: (Prime, Int)): BigInt = factor match {
    case (p, r) => p.totient(r)
  }

  /**
   * Method to calculate the Carmichael (or "reduced") totient for x.
   *
   * @param x a BigInt for which we require Carmichael's totient function.
   * @return a BigInt, which represents the Carmichael totient function.
   */
  def reducedTotient(x: BigInt): BigInt = primeFactorMultiplicity(x).foldLeft(BigInt(1)) { (lambda, factor) =>
    factor match {
      case (p, r) => Prime.lcm(lambda, reducedTotient(p -> r))
    }
  }

  /**
   * Carmichael's totient function for a prime power.
   * NOTE: often, this will be the same as Euler's totient function, but sometimes it will be one half.
   *
   * @param factor a tuple of Prime and exponent.
   * @return Carmichael's totient function for this Prime power.
   */
  def reducedTotient(factor: (Prime, Int)): BigInt = factor match {
    case (p, r) =>
      val phi = totient(factor)
      if (p.n == 2 && r >= 3) phi / 2 else phi
  }

  /**
   * Get the multiplicativeInverse for a BigInt (a) modulus n.
   *
   * NOTE: if is assumed that n is not a prime number. Assuming that n is coprime to a, then we return a.modPow(totient(n)-1), otherwise a.modInverse(n).
   *
   * @param a the number for which we need the multiplicative inverse.
   * @param n the modulus.
   * @return a number x such that ax is congruent to 1, mod n.
   */
  def multiplicativeInverse(a: BigInt, n: BigInt): BigInt = if coprime(n, a) then a.modPow(totient(n) - 1, n) else a.modInverse(n)

  /**
   * Method to yield the prime factors (with repeated elements).
   * NOTE that if x is prime, then the list returned consists of x (not sure why but that's how it's implemented in Scala 99).
   *
   * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
   *
   * @return a Seq[Prime].
   */
  def primeFactors(x: BigInt): Seq[Prime] = for ((k, v) <- primeFactorMultiplicity(x).toSeq; z <- Prime.fill(v)(k)) yield z

  /**
   * Method to yield a Map of prime factors.
   *
   * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
   *
   * @return a Map of Prime -=> Int where the Int represents the number of times the factor is multiplied.
   */
  def primeFactorMultiplicity(x: BigInt): Map[Prime, Int] =
    def factorCount(n: BigInt, p: Prime): (Int, Prime) =
      if (n % p.toBigInt != 0) (0, Prime(n))
      else factorCount(n / p.toBigInt, p) match {
        case (c, d) => (c + 1, d)
      }

    def factorsR(n: Prime, ps: LazyList[Prime]): Map[Prime, Int] =
      if (n.toBigInt == 1) Map()
      else if (n.isProbablePrime) Map(n -> 1)
      else {
        val nps = ps.dropWhile(n.toBigInt % _.toBigInt != 0)
        val (count, dividend) = factorCount(n.toBigInt, nps.head)
        Map(nps.head -> count) ++ factorsR(dividend, nps.tail)
      }

    factorsR(Prime(x), allPrimes)

  val commaFormatter = new java.text.DecimalFormat("#,###")

  def formatWithCommas(x: BigInt): String = commaFormatter.format(x)

  /**
   * Method to construct a new Prime, provided that it is positive.
   *
   * NOTE there is no check at all as to whether the result is actually a prime number.
   *
   * @param p a positive BigInt.
   * @return a Prime whose value may or may not be a Prime number.
   * @throws PrimeException if n is not positive.
   */
  def apply(p: BigInt): Prime = if p > 0 then new Prime(p) else throw PrimeException(s"prime must be positive ($p)")

  /**
   * Method to create a (probable) Prime from a String.
   *
   * @param s the String.
   * @return an optional Prime whose value is the String.
   */
  def create(s: String): Option[Prime] = create(BigInt(s))

  /**
   * Create an (optional) instance of Prime such that n is a probable prime.
   *
   * @param p a BigInt.
   * @return an Option[Prime]
   */
  def create(p: BigInt): Option[Prime] = optionalPrime(Prime(p))

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
   * Method to yield a Mersenne number: (2 to the power of n) - 1.
   *
   * @param p the prime number to generate the Mersenne prime.
   *          NOTE that no explicit check is made to ensure that n is prime.
   * @return a BigInt.
   */
  def mersenneNumber(p: Prime): BigInt = BigInt(2).pow(p.n.toInt) - 1

  /**
   * Method to detect if x has one of the smaller factors.
   * NOTE: we don't test for odd parity because we assume that x is odd.
   *
   * @param x an odd BigInt
   * @return true if 3 or 5 or 7 divides x.
   */
  def hasSmallFactor(x: BigInt): Boolean = (3 |> x) || (5 |> x) || (7 |> x)

  /**
   * Method to determine if n is a probable prime.
   * We use the MillerRabin test on n.
   * NOTE: we assume that n is odd.
   *
   * More or less the equivalent of n.isProbablePrime(100).
   * The performance appears to be similar.
   *
   * NOTE: you may be tempted to replace carmichael.contains(p) by isCarmichaelNumber(p) but tread very carefully if you do that!
   *
   * @param p an odd BigInt.
   * @return true if n is probably prime.
   */
  def isProbableOddPrime(p: BigInt): Boolean =
    hundredPrimes.contains(Prime(p)) || (p <= 7 || !hasSmallFactor(p)) && !carmichael.contains(p) && MillerRabin.isProbablePrime(p)

  /**
   * Method to determine if n is a probable prime.
   * We use the MillerRabin test on n.
   *
   * @param p a BigInt.
   * @return true if n is probably prime.
   */
  def isProbablePrime(p: BigInt): Boolean = (p == 2 || !(2 |> p)) && isProbableOddPrime(p)

  /**
   * Test n to determine if it is a Carmichael Number.
   *
   * @param n a BigInt to be tested.
   * @return true if n is a Carmichael Number.
   */
  def isCarmichaelNumber(n: BigInt): Boolean =
    carmichael.contains(n) || !hundredPrimes.contains(Prime(n)) && n != 1 && !(2 |> n) && carmichaelTheoremApplies(n)

  private def carmichaelTheoremApplies(n: BigInt) =
    val factors: Map[Prime, Int] = primeFactorMultiplicity(n)
    val tests = for ((p, r) <- factors) yield r == 1 && (n - 1) % (p.n - 1) == 0
    factors.size > 2 && tests.forall(p => p)

  /**
   * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
   *
   * @param n the number of copies to make.
   * @param x the value to be copied.
   * @tparam X the underlying type of the result.
   * @return a List[X].
   */
  private def fill[X](n: Int)(x: X): List[X] = {
    @tailrec
    def inner(r: List[X], l: Int): List[X] = if (l <= 0) r else inner(r :+ x, l - 1)

    inner(Nil, n)
  }

  private def getRandomValues(p: BigInt): Seq[BigInt] =
    val pMinus1 = p - 1
    val n = 20
    if (pMinus1 < n) Range(2, p.toInt).map(BigInt(_))
    else RandomState.lazyList(System.nanoTime()).map(_.value(pMinus1 - 1) + 2) take n
}

object Primes {

  /**
   * Method to yield the "prime counting function" aka "piApprox" for a given number.
   * The result is an approximation to the number of primes not greater than x.
   *
   * @param x the ordinal position in the list of primes of the prime number required.
   * @return an approximation to the kth prime number.
   */
  def piApprox(x: BigInt): Double = x.toDouble / math.log(x.toDouble)

  /**
   * Create a lazy list of BigInts starting with x.
   *
   * @param x the first BigInt that is required.
   * @return a LazyList[BigInt] whose head is x.
   */
  def bigInts(x: BigInt): LazyList[BigInt] = x #:: bigInts(x + 1)

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
  def probablePrimes(f: Prime => Boolean): List[Prime] =
    val result = probablePrimesLazy(f)
    if (result.knownSize == -1) result.toList
    else throw PrimeException("probablyPrimes: filter does not yield finite list")

  /**
   * Method to implement Eratosthenes Sieve.
   *
   * NOTE: this method uses an (mutable) Array and a mutable variable.
   *
   * @param m the largest number that we could get back as a prime.
   * @return a list of Prime numbers.
   */
  def eSieve(m: Int): List[Prime] =
    val sieve = new Array[Boolean](m + 1)
    var p = 2
    while (p < m) {
      for (i <- 2 to m / p) {
        val j = i * p
        if (!sieve(j)) sieve(j) = true
      }
      p += 1
      while (p <= m && sieve(p)) p += 1
    }
    val result: List[(Boolean, Int)] = sieve.to(List).zipWithIndex.drop(2).filterNot((x, _) => x)
    for (x <- result) yield Prime(x._2)

  /**
   * Method to yield a lazy list of all probable primes.
   *
   * @return a LazyList[Prime].
   */
  lazy val allPrimes: LazyList[Prime] = probablePrimesLazy(_ => true)

  /**
   * Method to yield a lazy list of all probable primes smaller than x.
   *
   * @return a LazyList[Prime].
   */
  def smallPrimes(x: BigInt): LazyList[Prime] = allPrimes takeWhile {
    _.n < x
  }

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
   * Random source.
   */
  private val random: java.util.Random = new java.util.Random()

  /**
   * Method to yield a lazy list of probable primes as long as they satisfy the predicate f.
   * As soon as f returns false, the lazy list terminates.
   *
   * @param f the predicate to be applied to each candidate prime.
   * @return a LazyList[Prime] where each element satisfies the predicate f.
   */
  private def probablePrimesLazy(f: Prime => Boolean): LazyList[Prime] =
    def inner(p: Prime): LazyList[Prime] = if (f(p)) p #:: inner(p.next) else LazyList.empty

    hundredPrimes.to(LazyList).filter(f) ++ inner(Prime(prime101))
}

/**
 * This code is from link shown below.
 *
 * It's not idiomatic Scala but I will clean it up as we go forward.
 */
object MillerRabin {
  // This code is attributed to: 'https://www.literateprograms.org/miller-rabin_primality_test__scala_.html'
  def miller_rabin_pass(a: BigInt, n: BigInt): Boolean =
    val (d, s) = decompose(n)
    var a_to_power: BigInt = a.modPow(d, n)
    if (a_to_power == 1) return true
    else for (_ <- 1 to s) {
      // TODO insert appropriate code here.
    }
    a_to_power == n - 1

  /**
   * Method (originally called miller_rabin) from "literateprograms" with slight improvements for elegance.
   *
   * NOTE: this is equivalent (I believe) to n.isProbablePrime(40),
   * i.e. there's a one-in-a-trillion chance that we get false positive.
   *
   * @param n a BigInt.
   * @return true if n is a probable prime with certainty approximately 2.pow(-40)
   */
  def isProbablePrime(n: BigInt): Boolean =
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

  private def decompose(n: BigInt) =
    var d: BigInt = n - 1
    var s: Int = 0
    while (2 |> d) {
      d >>= 1
      s += 1
    }
    (d, s)
}

object Goldbach {

  /**
   * Method to get a pair of primes which sum to a number.
   *
   * @param x an even number greater than 2.
   * @return a Try of a tuple of (p1, p2) where p1, p2 are primes such that p1 + p2 = x.
   */
  def goldbach(x: BigInt): Try[(Prime, Prime)] =
    if (x > 2 && x % 2 == 0) Try(doGoldbachEven(x))
    else Failure(new IllegalArgumentException("goldbach: input must be positive and even"))

  /**
   * Must be called with x>2 and even.
   *
   * @param x the number.
   * @return a tuple of (p1, p2) where p1, p2 are primes such that p1 + p2 = x.
   */
  private def doGoldbachEven(x: BigInt) =
    possiblePairs(x) find { (_, p2) => p2.isProbablePrime } match {
      case Some(p1, p2) => p1 -> p2
      case None => throw new IllegalArgumentException
    }

  private def possiblePairs(x: BigInt) = for (p <- smallPrimes(x)) yield p -> Prime(x - p.n)
}

case class PrimeException(str: String) extends Exception(str)

object Main extends App {

  val benchmark: Benchmark[BigInteger] = new Benchmark[BigInteger]("Eratosthenes", null, (t: BigInteger) => Primes.eSieve(t.intValue()), null)
  val time: Double = benchmark.run(BigInteger.valueOf(1000000), 10)
  println(s"Eratosthenes Sieve for 1000000 takes $time millisecs")
}