package crypto

/**
 * This defines a DiffieHellmanMerkle key exchange.
 *
 * For more information about primitive roots, see https://en.wikipedia.org/wiki/Primitive_root_modulo_n
 *
 * @param p a prime number.
 * @param g a primitive root of p.
 */
case class DiffieHellmanMerkle(p: Prime, g: BigInt) {
  // NOTE we should probably replace validate with isProbablyPrime for performance reasons.
  require(p.validate, s"$p is not prime")
  require(p.testPrimitiveRoot(g), s"$g is not a primitive root of $p")

  def encrypt(plain: BigInt)(secret: BigInt): BigInt = modPow(plain)(secret)

  def decrypt(cipher: BigInt)(secret: BigInt): BigInt = modPow(cipher)(multiplicativeInverse(secret))

  def keyExchange(privateKey: BigInt): BigInt = if (privateKey > 0 && privateKey < p.p) modPow(g)(privateKey) else throw PrimeException("privateKey must be positive and less than prime number")

  def secret(a: BigInt, b: BigInt): BigInt = {
    val ab = getSecret(keyExchange(a))(b)
    val ba = getSecret(keyExchange(b))(a)
    if (ab == ba) ab
    else throw PrimeException(s"secret: ab doesn't match ba")
  }

  /**
   * Get the modular multiplicative inverse of a number with respect to a prime number.
   *
   * For more information about the multiplicative inverse, see https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
   *
   * @param a the number for which we require the multiplicative inverse with respect to the prime number of this DiffieHellmanMerkle.
   * @return generalMultiplicativeInverse(a, p.p - 2)
   */
  def multiplicativeInverse(a: BigInt): BigInt = generalMultiplicativeInverse(a, p.p - 2)

  private def getSecret(receivedKey: BigInt)(privateKey: BigInt): BigInt = modPow(receivedKey)(privateKey)

  private def generalMultiplicativeInverse(a: BigInt, m: BigInt) = modPow(a)(m)

  /**
   * Method to determine base raised to the power with result modulo p (the prime of this DiffieHellmanMerkle).
   *
   * @param base  the base.
   * @param power the power.
   * @return the value of base raised to power, modulo p.
   */
  private def modPow(base: BigInt)(power: BigInt) = p.modPow(base, power)
}

object DiffieHellmanMerkle
