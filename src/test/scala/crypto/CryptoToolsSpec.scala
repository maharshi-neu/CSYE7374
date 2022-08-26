package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CryptoToolsSpec extends AnyFlatSpec with should.Matchers {

    behavior of "CryptoTools"

    it should "parseEnglish" in {
        val z: Option[Int] = CryptoTools.parseEnglish("HelloWorld!")
        z shouldBe Some(4) // matches "hell"
    }

    it should "dict" in {
        CryptoTools.triedDictionary.get.size shouldBe 235886
    }

    it should "isEnglishWord" in {
        CryptoTools.isEnglishWord("hello") shouldBe true
        CryptoTools.isEnglishWord("HELLO") shouldBe true
        CryptoTools.isEnglishWord("world") shouldBe true
        CryptoTools.isEnglishWord("World!") shouldBe false
    }

}
