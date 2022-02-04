package crypto

import java.math.BigInteger
import scala.util.Random

case class Prime(x: BigInt) extends AnyVal {
  def validate: Boolean = Primes.isPrime(x)
}

object Primes {
  def isPrime(x: BigInt): Boolean = MillerRabin.miller_rabin(x)
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