package crypto

import crypto.CaesarCipher.{doShift, preparePlainText}
import crypto.Histogram
import crypto.Histogram.{English, shiftedEnglishHistograms}
import parse.EnglishParser
import scala.util.Try
import scala.util.matching.Regex

/**
 * Case class to represent a Caesar cipher.
 *
 * @param shifts the shift for each character in the key.
 */
case class VigenereCipher(shifts: Seq[Int]) extends Cipher {
    val keyLength: Int = shifts.length

    def encrypt(w: CharSequence): CharSequence =
        (for ((c, i) <- preparePlainText(w.toString).zipWithIndex) yield doShift(c, shifts(i % keyLength))) mkString ""

    def decrypt(w: CharSequence): CharSequence =
        (for ((x, i) <- w.toString.zipWithIndex) yield doShift(x, -shifts(i % keyLength))) mkString ""
}

object VigenereCipher {

    def apply(key: String): VigenereCipher = VigenereCipher(for (x <- key) yield x - 'A')
}

