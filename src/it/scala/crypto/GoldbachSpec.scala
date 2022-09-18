package crypto

import crypto.Goldbach.goldbach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Success

class GoldbachSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Goldbach"

    it should "goldbach1" in {
        goldbach(10) shouldBe Success(Prime(3), Prime(7))
    }
    it should "goldbach2" in {
        goldbach(100) shouldBe Success(Prime(3), Prime(97))
    }
    it should "goldbach3" in {
        goldbach(1000) shouldBe Success(Prime(3), Prime(997))
    }
    it should "goldbach4" in {
        goldbach(10000) shouldBe Success((Prime(59), Prime(9941)))
    }
    it should "goldbach5" in {
        goldbach(100000) shouldBe Success((Prime(11), Prime(99989)))
    }
    it should "goldbach6" in {
        goldbach(1000000) shouldBe Success((Prime(17), Prime(999983)))
    }
    it should "goldbach7" in {
        goldbach(10000000) shouldBe Success((Prime(29), Prime(9999971)))
    }
    it should "goldbach8" in {
        goldbach(100000000) shouldBe Success((Prime(11), Prime(99999989)))
    }
    it should "goldbach9" in {
        goldbach(1000000000) shouldBe Success((Prime(71), Prime(999999929)))
    }
    it should "goldbach10" in {
        goldbach(10000000000L) shouldBe Success((Prime(71), Prime(9999999929L)))
    }
    it should "goldbach11" in {
        goldbach(100000000000L) shouldBe Success((Prime(23), Prime(99999999977L)))
    }
    it should "goldbach12" in {
        goldbach(1000000000000L) shouldBe Success((Prime(11), Prime(999999999989L)))
    }
    it should "goldbach13" in {
        goldbach(10000000000000L) shouldBe Success((Prime(29), Prime(9999999999971L)))
    }

}
