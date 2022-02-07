package crypto

import crypto.RandomState.lazyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RandomStateSpec extends AnyFlatSpec with should.Matchers {

  behavior of "RandomStateSpec"

  it should "next" in {
    val r = RandomState(0L).next
    r.value(BigInt(1000)) shouldBe 339
  }

  it should "lazyList" in {
    val rs = lazyList(0L)
    val xs = rs.map(_.value(BigInt(1000))).take(10).toList
    xs.head shouldBe BigInt(180)
    xs.tail.head shouldBe BigInt(162)
  }

}
