package crypto

import java.math.BigInteger
import scala.util.Random

/**
 * Class to represent a prime number.
 *
 * @param x the value of the prime number.
 */
case class Prime(x: BigInt) extends AnyVal {
  /**
   * Validate whether this number really is prime.
   *
   * @return true if this number is prime.
   */
  def validate: Boolean = x.intValue == 2 || {
    val max = math.sqrt(x.toDouble)
    val candidates: List[Prime] = Primes.primes(p => p.toDouble < max).toList
    val factors: List[Prime] = candidates filter (f => x % f.x == 0)
    factors.toList.isEmpty
  }

  /**
   * Get the remainder from the division y/x.
   *
   * @param y a BigInt.
   * @return y % x.
   */
  def remainder(y: BigInt): BigInt = y % x

  /**
   * Get the value of this prime.
   *
   * @return x.
   */
  def toBigInt: BigInt = x

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
   * Get the next prime number after this one.
   *
   * @return a Prime which is greater than this.
   */
  def next: Prime = {
    var d = x + 2
    while (!Primes.isProbablePrime(d)) d = d + 2
    Prime(d)
  }
}

object Prime {
  /**
   * Method to create a Prime from a String.
   *
   * @param x the String.
   * @return a Prime whose value is the String.
   */
  def apply(x: String): Prime = Prime(BigInt(x))

  def create(x: BigInt): Option[Prime] = if (Primes.isProbablePrime(x)) Some(Prime(x)) else None
}

object Primes {
  def isProbablePrime(x: BigInt): Boolean = MillerRabin.miller_rabin(x)

  def primes(f: Prime => Boolean): LazyList[Prime] = {
    def all(p: Prime): LazyList[Prime] = if (f(p)) (p #:: all(p.next)) else LazyList.empty

    Prime(2) #:: all(Prime(3))
  }

  def allPrimes: LazyList[Prime] = primes(_ => true)
}

/**
 * This code is from https://www.literateprograms.org/miller-rabin_primality_test__scala_.html
 *
 * It's not idiomatic Scala but I will clean it up as we go forward.
 */
object MillerRabin {
  def miller_rabin_pass(a: BigInt, n: BigInt): Boolean = {
    var d: BigInt = 0
    var s: BigInt = 0
    var a_to_power: BigInt = 0
    var i: Int = 0
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
    for (i <- 1 to s.intValue) {
      if (a_to_power == n - 1) {
        return true
      }
      a_to_power = (a_to_power * a_to_power) % n
    }
    return (a_to_power == n - 1)
  }

  def miller_rabin(n: BigInt): Boolean = {
    var k: Int = 20
    for (i: Int <- 1 to k) {
      var a: BigInt = 0
      var rand: scala.util.Random = new Random(new java.util.Random())
      while (a == 0) {
        a = new BigInt(new BigInteger("" + (rand.nextDouble * n.doubleValue).toInt))
      }
      if (!miller_rabin_pass(a, n)) {
        return false
      }
    }
    return true
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