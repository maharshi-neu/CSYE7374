package util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TwisteratorSpec extends AnyFlatSpec with should.Matchers {

    behavior of "TwisteratorSpec"

    val twister1: Iterator[Int] => Twisterator[Int, Int] = Twisterator[Int](x => -x)(_)

    it should "twist1" in {
        val list = List(1, 2, 3)
        val result = for (x <- twister1(list.iterator)) yield x
        result.next() shouldBe 1
        result.next() shouldBe 2
        result.next() shouldBe -3
        result.hasNext shouldBe false
    }

    val twister2: Iterator[Int] => Twisterator[Int, String] = Twisterator[Int, String](_.toString, x => s"-$x")(_)

    it should "twist2" in {
        val list = List(1, 2, 3)
        val result = for (x <- twister2(list.iterator)) yield x
        result.next() shouldBe "1"
        result.next() shouldBe "2"
        result.next() shouldBe "-3"
        result.hasNext shouldBe false
    }
}
