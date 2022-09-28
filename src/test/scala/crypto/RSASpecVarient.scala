package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Random

class RSASpecVarient extends AnyFlatSpec with should.Matchers {

    behavior of "RSASpec"

    implicit val random: Random = new Random(7)

    // For the following example, see https://en.wikipedia.org/wiki/RSA_(cryptosystem)
    private val wikiRSA: RSA = RSA(Prime(79), Prime(97), (_, _) => 2)
    private val msgPlain = 69
    private val msgCipher = 6809

    it should "encrypt" in {
        wikiRSA.encrypt(msgPlain) shouldBe msgCipher
    }

    it should "decrypt" in {
        wikiRSA.decrypt(msgCipher) shouldBe msgPlain
    }

    it should "apply" in {
        val target = wikiRSA
        target.publicKey shouldBe Key(7663, Prime(11))
        target.privateKey shouldBe Key(7663, Prime(227))
    }

    // FIXME
    ignore should "roundTrip" in {
        val target = RSA.apply
        target.encrypt(msgPlain) shouldBe msgCipher
        target.decrypt(msgCipher) shouldBe msgPlain
    }

}
