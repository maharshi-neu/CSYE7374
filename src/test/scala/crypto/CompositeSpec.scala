package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CompositeSpec extends AnyFlatSpec with should.Matchers {

  behavior of "CompositeSpec"

  it should "ady" in {
    val composite: Composite = Composite(70)
    composite.ady shouldBe 3
    composite.factors shouldBe Seq(Prime(2), Prime(5), Prime(7))
  }

}
