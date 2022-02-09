package crypto

/**
 * This defines a DiffieHellmanMerkle key exchange.
 *
 * @param p a prime number.
 * @param g a primitive root of p.
 */
case class DiffieHellmanMerkle(p: Prime, g: BigInt) {
  require(p.validate, s"$p is not prime")
  require(p.testPrimitiveRoot(g), s"$g is not a primitive root of $p")

  def encrypt(clear: BigInt)(secret: BigInt): BigInt = modPow(clear)(secret)

  def decrypt(cipher: BigInt)(secret: BigInt): BigInt = modPow(cipher)(multInverse(secret))

  def keyExchange(privateKey: BigInt): BigInt = modPow(g)(privateKey)

  def getSecret(receivedKey: BigInt)(privateKey: BigInt): BigInt = modPow(receivedKey)(privateKey)

  def secret(a: BigInt, b: BigInt): BigInt = {
    val ab = getSecret(keyExchange(a))(b)
    val ba = getSecret(keyExchange(b))(a)
    if (ab == ba) ab
    else throw PrimeException(s"secret: ab doesn't match ba")
  }

  def multInverse(a: BigInt): BigInt = modPow(a)(p.p - 2)

  private def modPow(base: BigInt)(key: BigInt) = p.modPow(base, key)
}

object DiffieHellmanMerkle
