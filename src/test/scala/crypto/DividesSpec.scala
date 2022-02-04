package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.language.postfixOps

class DividesSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Divides"

    it should "work for Int" in {
        1 |> 2 shouldBe true
        2 |> 4 shouldBe true
        3 |> 39 shouldBe true
        7 |> 49 shouldBe true
    }
    it should "work for Long" in {
        1 |> 2L shouldBe true
        2 |> 4L shouldBe true
        3 |> 39L shouldBe true
        7 |> 49L shouldBe true
    }
    it should "work for BigInt" in {
        1 |> BigInt(2) shouldBe true
        2 |> BigInt(4) shouldBe true
        3 |> BigInt(39) shouldBe true
        7 |> BigInt(49) shouldBe true
    }
}
