package crypto

import crypto.Prime.reducedTotient
import crypto.Primes.smallPrimes
import java.math.BigInteger
import scala.util.Random


trait RawCipher {
  def encrypt(plain: BigInt): BigInt

    def decrypt(cipher: BigInt): BigInt
}

case class Key(n: BigInt, p: Prime) {
    def apply(x: BigInt): BigInt = x.modPow(p.toBigInt, n)
}

case class RSA(publicKey: Key, privateKey: Key) extends RawCipher {
    /**
     * Method to encrypt a BigInt message using the public key.
     *
     * @param plain the plain message.
     * @return the cipher message.
     */
    def encrypt(plain: BigInt): BigInt = publicKey(plain)

    /**
     * Method to decrypt a BigInt message using the private key.
     *
     * @param cipher the cipher message.
     * @return the plain message.
     */
    def decrypt(cipher: BigInt): BigInt = privateKey(cipher)
}

object RSA {
    val random = new Random()

    def apply(p: Prime, q: Prime, f: (Random, Int) => Int): RSA = {
        require(p.validate)
        require(q.validate)

        val n: BigInt = p * q
        val totient: BigInt = reducedTotient(n)
        val primes: List[Prime] = smallPrimes(totient).toList.filter(p => p.validate).filter(p => p.isCoprimeTo(totient))
        val e: Prime = primes(f(random, primes.length))
        val d: Prime = ??? // TODO implement me
        val z = ??? // TODO implement me
        assert(z == 1)

        val publicKey = Key(n, e)
        val privateKey = Key(n, d)

        RSA(publicKey, privateKey)
    }

    def apply(p: Prime, q: Prime): RSA = apply(p, q, (r, n) => r.nextInt(n))

    def apply: RSA = {
        val p = Primes.randomPrime(12)
        val q = Primes.randomPrime(12)
        RSA(p, q)
    }
}
