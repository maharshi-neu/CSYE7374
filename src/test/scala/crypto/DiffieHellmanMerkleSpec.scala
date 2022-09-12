package crypto

import java.math.BigInteger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DiffieHellmanMerkleSpec extends AnyFlatSpec with should.Matchers {

  behavior of "DiffieHellmanMerkle"

  private val prime: Prime = Prime(23)
  private val g: BigInt = 5 // a primitive root of 23 (the others are 7, 10, 11, 14, 15, 17, 19, 20, 21)
  private val aliceKey: BigInt = 4
  private val bobKey: BigInt = 3
  private val secret: BigInt = 18
  private val plainText: BigInt = 17
  private val cipherText: BigInt = 3


  it should "construct" in {
    val target = DiffieHellmanMerkle(prime, g)
    target.modulus shouldBe prime
    target.generator shouldBe g
    target.toString shouldBe s"DiffieHellmanMerkle($prime,$g)"
  }

  it should "not construct" in {
    a[java.lang.IllegalArgumentException] should be thrownBy DiffieHellmanMerkle(Prime(4), 0)
    a[java.lang.IllegalArgumentException] should be thrownBy DiffieHellmanMerkle(prime, 4)
  }

  it should "keyExchange" in {
    val target = DiffieHellmanMerkle(prime, g)
    target.keyExchange(aliceKey) shouldBe 4
    target.keyExchange(bobKey) shouldBe 10
  }

  it should "secret" in {
    val target = DiffieHellmanMerkle(prime, g)
    target.secret(aliceKey, bobKey) shouldBe secret
  }

  it should "get the multiplicative inverse" in {
    val target = DiffieHellmanMerkle(prime, g)
    val s = target.secret(aliceKey, bobKey)
    s shouldBe secret
    val multiplicativeInverse = target.multiplicativeInverse(s)
    multiplicativeInverse shouldBe 9
    val product: BigInt = s * multiplicativeInverse
    product.mod(prime.toBigInt) shouldBe 1
    product shouldBe 162
    product shouldBe (prime.toBigInt * 7 + 1)
  }

  it should "modPow" in {
    val z = prime.modPow(plainText, secret)
    z shouldBe plainText.pow(secret.toInt).mod(prime.toBigInt)
    z shouldBe cipherText
  }

  it should "encrypt" in {
    val target = DiffieHellmanMerkle(prime, g)
    val s = target.secret(aliceKey, bobKey)
    s shouldBe secret
    target.encrypt(plainText)(s) shouldBe cipherText
    val ciphers = for (i <- 1 to 22) yield target.encrypt(i)(s)
    println(ciphers)
  }

  // FIXME
  ignore should "modPow 2" in {
    val inverseSecret = DiffieHellmanMerkle(prime, g).multiplicativeInverse(secret)
    inverseSecret shouldBe 9
    val cipherText = prime.modPow(plainText, secret)
    cipherText shouldBe plainText.pow(secret.toInt).mod(prime.toBigInt)
    val recoveredText: BigInt = prime.modPow(cipherText, inverseSecret)
    recoveredText shouldBe cipherText.pow(inverseSecret.toInt).mod(prime.toBigInt)
    recoveredText shouldBe plainText
  }

  // FIXME
  ignore should "decrypt" in {
    val target = DiffieHellmanMerkle(prime, g)
    val s = target.secret(aliceKey, bobKey)
    s shouldBe secret
    target.decrypt(cipherText)(s) shouldBe plainText
  }
}
