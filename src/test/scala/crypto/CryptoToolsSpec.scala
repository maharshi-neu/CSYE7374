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
        CryptoTools.triedDictionary.get.size shouldBe 235884
    }

    it should "isEnglishWordExactCase" in {
        CryptoTools.isEnglishWordExactCase("hello") shouldBe true
        CryptoTools.isEnglishWordExactCase("HELLO") shouldBe false
        CryptoTools.isEnglishWordExactCase("World!") shouldBe false
        CryptoTools.isEnglishWordExactCase("Edward") shouldBe true
    }

    it should "isEnglishWordAnyCase" in {
        CryptoTools.isEnglishWordAnyCase("hello") shouldBe true
        CryptoTools.isEnglishWordAnyCase("HELLO") shouldBe true
        CryptoTools.isEnglishWordAnyCase("world") shouldBe true
        CryptoTools.isEnglishWordAnyCase("World!") shouldBe false
    }

}
