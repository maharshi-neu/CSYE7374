package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DiffieHellmanMerkleFuncSpec extends AnyFlatSpec with should.Matchers {

  behavior of "DiffieHellmanMerkle"

  private val prime23: Prime = Prime(23)
  private val g: BigInt = 5
  private val angelaKey: BigInt = 4
  private val brianKey: BigInt = 3
  private val aliceKey: BigInt = 6
  private val bobKey: BigInt = 15

  it should "construct" in {
    val target = DiffieHellmanMerkle(prime23, g)
    target.p shouldBe prime23
    target.g shouldBe g
  }

  it should "not construct" in {
    a[java.lang.IllegalArgumentException] should be thrownBy DiffieHellmanMerkle(Prime(4), 0)
    a[java.lang.IllegalArgumentException] should be thrownBy DiffieHellmanMerkle(prime23, 4)
  }

  it should "keyExchange" in {
    val target = DiffieHellmanMerkle(prime23, g)
    target.keyExchange(angelaKey) shouldBe 4
    target.keyExchange(brianKey) shouldBe 10
    target.keyExchange(aliceKey) shouldBe 8
    target.keyExchange(bobKey) shouldBe 19
  }

  it should "secret" in {
    val target = DiffieHellmanMerkle(prime23, g)
    target.secret(angelaKey, brianKey) shouldBe 18
    target.secret(aliceKey, bobKey) shouldBe 2
  }

  ignore should "encrypt" in {
    val target = DiffieHellmanMerkle(prime23, g)
    val secret = target.secret(aliceKey, bobKey)
    target.encrypt(17)(secret) shouldBe 3

    val ciphers = for (i <- 0 to 22) yield target.encrypt(i)(secret)
    println(ciphers)
  }

  ignore should "decrypt" in {
    val target = DiffieHellmanMerkle(prime23, g)
    target.decrypt(3)(target.secret(aliceKey, bobKey)) shouldBe 17
  }
}
