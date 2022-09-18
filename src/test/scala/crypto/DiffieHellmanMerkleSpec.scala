package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.math.BigInteger
import scala.util.Success

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
    target.secret(aliceKey, bobKey) shouldBe Success(secret)
  }
}
