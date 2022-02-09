package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DiffieHellmanMerkleSpec extends AnyFlatSpec with should.Matchers {

  behavior of "DiffieHellmanMerkle"

  private val prime23: Prime = Prime(23)
  private val g: BigInt = 5
  private val aliceKey: BigInt = 4
  private val bobKey: BigInt = 3

  it should "construct" in {
    val target = DiffieHellmanMerkle(prime23, g)
    target.p shouldBe prime23
    target.g shouldBe g
  }

  it should "not construct" in {
    a[java.lang.IllegalArgumentException] should be thrownBy DiffieHellmanMerkle(Prime(4), 0)
    a[java.lang.IllegalArgumentException] should be thrownBy DiffieHellmanMerkle(prime23, 4)
  }

  it should "message" in {
    val target = DiffieHellmanMerkle(prime23, g)
    // Alice's secret is 4
    target.keyExchange(aliceKey) shouldBe 4
    // Bob's secret is 3
    target.keyExchange(bobKey) shouldBe 10
  }

  it should "secret" in {
    val target = DiffieHellmanMerkle(prime23, g)
    target.secret(aliceKey, bobKey) shouldBe 18
  }

  it should "encrypt" in {
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
