package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class VigenereCipherSpec extends AnyFlatSpec with should.Matchers {

    private val cipher = VigenereCipher("ABCD")

    behavior of "VigenereCipher"

    it should "encrypt" in {
        //noinspection SpellCheckingInspection
        cipher.encrypt("Hello World") shouldBe "HFNOOXQULE"
    }

    it should "decrypt" in {
        //noinspection SpellCheckingInspection
        cipher.decrypt("HFNOOXQULE") shouldBe "HELLOWORLD"
    }

    it should "apply" in {
        VigenereCipher("ABCD") shouldBe new VigenereCipher(Seq(0, 1, 2, 3))
    }

}
