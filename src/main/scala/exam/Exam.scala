package exam

import scala.annotation.tailrec
import scala.collection.SortedSet
import scala.util.{Failure, Random, Try}

object Exam {

    /**
     * Method to find the greatest common divisor of two Long numbers, a and b.
     * If b is 0, then return a.
     * Else return the gcd of b and a modulo b.
     * Your method must be tail-recursive.
     *
     * @param a the larger of two Long numbers.
     * @param b the smaller of two Long numbers.
     * @return the greatest common divisor of a and b.
     */
    @tailrec
    def gcd(a: BigInt, b: BigInt): BigInt = /* XXX IMPLEMENT ME */ if (b == 0) a else gcd(b, a % b) /* XXX END IMPLEMENTATION */

    def divides(f: Long, n: BigInt): Boolean = n % f == 0

    def isOdd(n: BigInt): Boolean = !divides(2, n)

    def satisfiesFermatsTest(a: BigInt, n: BigInt): Boolean = a.modPow(n - 1, n) == BigInt(1)

    def likelyPrime(n: BigInt): Boolean = /* XXX IMPLEMENT ME */ {
        val m = 5
        val nPrimes = hundredPrimes.count(_ < n)
        val randomState = RandomState.intRandomStateBoundedSeeded(nPrimes)(0L)
        val candidates = randomState.lazyList.take(m).map(x => hundredPrimes(x))
        candidates.forall(a => satisfiesFermatsTest(a, n))
    }
    /* XXX END IMPLEMENTATION */

    def isPrime(x: BigInt): Boolean = x == 2 || isOdd(x) || hundredPrimes.contains(x) || likelyPrime(x)

    /**
     * The first 100 true primes.
     */
    val hundredPrimes: Seq[BigInt] =
        Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
            31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
            73, 79, 83, 89, 97, 101, 103, 107, 109, 113,
            127, 131, 137, 139, 149, 151, 157, 163, 167, 173,
            179, 181, 191, 193, 197, 199, 211, 223, 227, 229,
            233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
            283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
            353, 359, 367, 373, 379, 383, 389, 397, 401, 409,
            419, 421, 431, 433, 439, 443, 449, 457, 461, 463,
            467, 479, 487, 491, 499, 503, 509, 521, 523, 541).map(BigInt(_))

}
//
//trait RNG[X] {
//    val value: X
//    val next: RNG[X]
//    val lazyList: LazyList[X] = LazyList.cons[X](value, next.lazyList) //value #:: next.lazyList
//    def map[Y](f: X => Y): RNG[Y]
//}
//
//case class RNG_Random[X](n: Long)(g: Long => X) extends RNG[X] {
//    val value: X = g(n)
//    val next: RNG[X] = RNG_Random[X](new Random(n).nextLong())(g)
//    def map[Y](f: X => Y): RNG[Y] = RNG_Random(n)(g andThen f)
//}
//
//object RNG {
//    def applyLong: RNG[Long] = RNG_Random(System.currentTimeMillis())(identity)
//    def applyBigInt: RNG[BigInt] = RNG_Random(System.currentTimeMillis())(BigInt(_))
//}