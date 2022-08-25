package crypto

import crypto.CaesarCipher.{doShift, removeWhiteSpace}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CaesarCipherSpec extends AnyFlatSpec with should.Matchers {

    behavior of "CaesarCipher"

    it should "encrypt 0" in {
        val cipher = CaesarCipher(0)
        cipher.encrypt("Hello World!") shouldBe "HELLOWORLD"
    }
    it should "decrypt 0" in {
        val cipher = CaesarCipher(0)
        cipher.decrypt("HELLOWORLD") shouldBe "HELLOWORLD"
    }
    it should "encrypt 1" in {
        val cipher = CaesarCipher(1)
        //noinspection SpellCheckingInspection
        cipher.encrypt("Hello World!") shouldBe "IFMMPXPSME"
    }
    it should "decrypt 1" in {
        val cipher = CaesarCipher(1)
        //noinspection SpellCheckingInspection
        cipher.decrypt("IFMMPXPSME") shouldBe "HELLOWORLD"
    }
    it should "removeWhiteSpace" in {
        removeWhiteSpace("Hello World!") shouldBe "HelloWorld"
    }
    it should "doShift up" in {
        doShift('A', 0) shouldBe 'A'
        doShift('A', 1) shouldBe 'B'
        doShift('Z', 1) shouldBe 'A'
    }
    it should "doShift down" in {
        doShift('A', -1) shouldBe 'Z'
        doShift('Z', -1) shouldBe 'Y'
    }

}