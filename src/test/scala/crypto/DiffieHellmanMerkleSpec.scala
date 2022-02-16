package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DiffieHellmanMerkleSpec extends AnyFlatSpec with should.Matchers {

  behavior of "DiffieHellmanMerkle"

  private val prime11: Prime = Prime(11)
  private val g: BigInt = 2 // a primitive root because 2^10 mod 11 = 1
  private val aliceKey: BigInt = 8
  private val bobKey: BigInt = 4

  it should "construct" in {
    val target = DiffieHellmanMerkle(prime11, g)
    target.p shouldBe prime11
    target.g shouldBe g
  }

  it should "not construct" in {
    a[java.lang.IllegalArgumentException] should be thrownBy DiffieHellmanMerkle(Prime(4), 0)
    a[java.lang.IllegalArgumentException] should be thrownBy DiffieHellmanMerkle(prime11, 4)
  }

  it should "keyExchange" in {
    val target = DiffieHellmanMerkle(prime11, g)
    target.keyExchange(aliceKey) shouldBe 3
    target.keyExchange(bobKey) shouldBe 5
  }

  it should "secret" in {
    val target = DiffieHellmanMerkle(prime11, g)
    target.secret(aliceKey, bobKey) shouldBe 4
  }

  ignore should "encrypt" in {
    val target = DiffieHellmanMerkle(prime11, g)
    val secret = target.secret(aliceKey, bobKey)
    target.encrypt(17)(secret) shouldBe 3

    val ciphers = for (i <- 0 to 22) yield target.encrypt(i)(secret)
    println(ciphers)
  }

  ignore should "decrypt" in {
    val target = DiffieHellmanMerkle(prime11, g)
    target.decrypt(3)(target.secret(aliceKey, bobKey)) shouldBe 17
  }
}
