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
    target.encrypt(17)(target.secret(aliceKey, bobKey)) shouldBe bobKey
  }

  ignore should "decrypt" in {
    val target = DiffieHellmanMerkle(prime23, g)
    target.decrypt(bobKey)(target.secret(aliceKey, bobKey)) shouldBe 17
  }

  it should "multInverse" in {
    DiffieHellmanMerkle(Prime(11), 2).multInverse(3) shouldBe 4
    DiffieHellmanMerkle(Prime(17), 3).multInverse(10) shouldBe 12
    DiffieHellmanMerkle(prime23, 5).multInverse(18) shouldBe 9
  }
}
