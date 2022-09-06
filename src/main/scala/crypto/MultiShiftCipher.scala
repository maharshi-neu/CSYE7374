package crypto

import crypto.CaesarCipher.{doShift, preparePlainText}
import crypto.Histogram
import crypto.Histogram.{English, shiftedEnglishHistograms}
import parse.EnglishParser
import scala.util.{Random, Try}
import scala.util.matching.Regex

/**
 * Case class to represent a Substitution cipher with multiple shifts.
 *
 * @param shifts the shift for each character in the key.
 */
case class MultiShiftCipher(shifts: Seq[Int]) extends Cipher {
    val keyLength: Int = shifts.length

    def encrypt(w: CharSequence): CharSequence =
        ??? // TODO implement me

    def decrypt(w: CharSequence): CharSequence =
        ??? // TODO implement me
}

object MultiShiftCipher {
    def VigenereCipher(key: String): MultiShiftCipher = MultiShiftCipher(for (x <- key.toUpperCase) yield x - 'A')

    def OneTimePad(random: Random, n: Int): MultiShiftCipher =
        MultiShiftCipher(LazyList.continually(random.nextInt(26)).take(n).toList)
}

