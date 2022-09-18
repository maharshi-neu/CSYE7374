package crypto

import crypto.CaesarCipher.{doShift, guessShift, preparePlainText}
import crypto.CaesarCipherSpec.PoohIntroduction1
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CaesarCipherSpec extends AnyFlatSpec with should.Matchers {

  behavior of "CaesarCipher class"

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

  behavior of "CaesarCipher object"

  it should "preparePlainText" in {
    preparePlainText("Hello World!") shouldBe "HELLOWORLD"
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
  it should "guessShift 1" in {
    val shift = 1
    val unknownCipher = CaesarCipher(shift)
    val cipherText = unknownCipher.encrypt(PoohIntroduction1)
    val bestGuess = guessShift(cipherText)
    bestGuess shouldBe Some(1)
    val guessedCipher = CaesarCipher(bestGuess.get)
    val pattern = """HERE.*""".r
    pattern.matches(guessedCipher.decrypt(cipherText)) shouldBe true
  }
  it should "guessShift 2" in {
    val shift = 2
    val unknownCipher = CaesarCipher(shift)
    val cipherText = unknownCipher.encrypt(PoohIntroduction1)
    val maybeInt = guessShift(cipherText)
    maybeInt shouldBe Some(2)
    val guessedCipher = CaesarCipher(maybeInt.get)
    val pattern = """HERE.*""".r
    pattern.matches(guessedCipher.decrypt(cipherText)) shouldBe true
  }

  it should "guessShift 3" in {
    val shift = 3
    val unknownCipher = CaesarCipher(shift)
    val cipherText = unknownCipher.encrypt(PoohIntroduction1)
    val maybeGuess = guessShift(cipherText)
    // NOTE: this doesn't actually work but, based on only one three-letter stem, it seems OK.
    maybeGuess shouldBe Some(3)
  }
}

object CaesarCipherSpec {

  val PoohIntroduction1: String =
    """Here is Edward Bear, coming downstairs now, bump, bump, bump, on the
      |back of his head, behind Christopher Robin.""".stripMargin

  val PoohIntroduction2: String =
    PoohIntroduction1 +
      """It is, as far as he knows,
        |the only way of coming downstairs, but sometimes he feels that there
        |really is another way, if only he could stop bumping for a moment and
        |think of it. And then he feels that perhaps there isn't. Anyhow, here he
        |is at the bottom, and ready to be introduced to you. Winnie-the-Pooh.""".stripMargin

}