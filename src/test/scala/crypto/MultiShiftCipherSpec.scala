package crypto

import crypto.MultiShiftCipher.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Random

class MultiShiftCipherSpec extends AnyFlatSpec with should.Matchers {

    private val cipher = VigenereCipher("ABCD")

    behavior of "MultiShiftCipher"

    it should "encrypt" in {
        //noinspection SpellCheckingInspection
        cipher.encrypt("Hello World") shouldBe "HFNOOXQULE"
    }

    it should "decrypt" in {
        //noinspection SpellCheckingInspection
        cipher.decrypt("HFNOOXQULE") shouldBe "HELLOWORLD"
    }

    it should "VigenereCipher" in {
        VigenereCipher("ABCD") shouldBe new MultiShiftCipher(Seq(0, 1, 2, 3))
    }

    it should "OneTimePad" in {
        val oneTimePad = OneTimePad(new Random(), 20)
        oneTimePad.shifts.length shouldBe 20
    }

}
