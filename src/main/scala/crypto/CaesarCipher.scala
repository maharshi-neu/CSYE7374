package crypto

import crypto.CaesarCipher.{doShift, removeWhiteSpace}
import scala.util.matching.Regex

case class CaesarCipher(shift: Int) {
    def encrypt(w: String): String =
        for (c <- removeWhiteSpace(w)) yield doShift(c, shift)

    def decrypt(w: String): String = for (x <- w) yield doShift(x, -shift)
}

object CaesarCipher {

    def removeWhiteSpace(w: String): String = {
        val sb = new StringBuilder()
        for (m <- """\w+""".r findAllMatchIn w; x <- m.group(0)) sb.append(x)
        sb.toString()
    }

    private val A = 'A'

    private val radix = 26

    def doShift(x: Char, shift: Int): Char = ((x.toUpper.toInt + shift - A + radix) % radix + A).toChar
}
