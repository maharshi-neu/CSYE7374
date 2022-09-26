package util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TwisteratorSpec extends AnyFlatSpec with should.Matchers {

    behavior of "TwisteratorSpec"

    val twister: Iterator[Int] => Twisterator[Int] = new Twisterator[Int](x => -x)(_)

    it should "twist" in {
        val list = List(1, 2, 3)
        val result = for (x <- twister(list.iterator)) yield x
        result.next() shouldBe 1
        result.next() shouldBe 2
        result.next() shouldBe -3
        result.hasNext shouldBe false
    }
}
