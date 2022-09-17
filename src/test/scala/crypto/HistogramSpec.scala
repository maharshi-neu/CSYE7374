package crypto

import crypto.Histogram.English
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class HistogramSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Histogram"

  it should "comparer" in {
    val histogram = Histogram(CaesarCipher.preparePlainText(CaesarCipherSpec.PoohIntroduction2))
    English.comparer(histogram) shouldBe 0.031 +- 0.004
  }

  it should "rotate" in {
    val histogram = Histogram(CaesarCipher.preparePlainText(CaesarCipherSpec.PoohIntroduction2)).rotate(x => CaesarCipher.doShift(x, 1))
    histogram.get('B') shouldBe Some(23)
    English.comparer(histogram) shouldBe 0.041 +- 0.004
  }

  it should "apply 1" in {
    Histogram("A") shouldBe Histogram(Map('A' -> 1))
    //noinspection SpellCheckingInspection
    Histogram("IFMMPXPSME").map.size shouldBe 7
  }

  it should "apply 2" in {
    val plainText = CaesarCipher.preparePlainText(CaesarCipherSpec.PoohIntroduction2)
    plainText.distinct.length shouldBe 21
    val histogram = Histogram(plainText)
    histogram.get('A') shouldBe Some(23)
    histogram.get('Y') shouldBe Some(8)
    histogram.get('Z') shouldBe None
    val keys = histogram.keySet
    keys.size shouldBe 21
    keys shouldBe Set('E', 'N', 'T', 'Y', 'U', 'F', 'A', 'M', 'I', 'G', 'L', 'B', 'P', 'C', 'H', 'W', 'O', 'D', 'S', 'K', 'R')
  }

}
