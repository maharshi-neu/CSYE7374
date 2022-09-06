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
 * TODO consider re-writing this in terms of MultiShiftCipher with one shift only.
 *
 * @param shift the number of places each letter will be shifted.
 */
case class CaesarCipher(shift: Int) extends Cipher {
    def encrypt(w: CharSequence): CharSequence = ??? // TODO implement me

    def decrypt(w: CharSequence): CharSequence = ??? // TODO implement me
}

object CaesarCipher {

    /**
     * Method to remove white space and force to upper case.
     *
     * @param w the string to be prepared.
     * @return a string consisting only of the characters A...Z.
     */
    def preparePlainText(w: String): String = {
        val sb = new StringBuilder()
        for (m <- """\w+""".r findAllMatchIn w.toUpperCase; x <- m.group(0)) sb.append(x)
        sb.toString()
    }

    def showCipherText(w: String): String = {
        w // TODO put into blocks of five
    }

    /**
     * Method to guess the best candidates for Caesar Cipher shift that could have produced the given Histogram.
     *
     * @param histogram the histogram (of a cipher text) for which you want the best (Caesar) shifts.
     * @return a sorted list of tbe best shift guesses.
     */
    def bestShifts(histogram: Histogram): Seq[Int] = {
        val (_, shifts) = shiftedEnglishHistograms.sortBy((h, _) => histogram.comparer(h)).unzip
        shifts
    }

    /**
     * Method to guess the value of the Caesar Cipher shift.
     * NOTE: this is only a guess. It isn't any sort of guarantee.
     *
     * @param cipherText the cipher text which you would like to decrypt.
     * @return an optional Int representing the best shift. If None, then we couldn't find any suitable shift.
     */
    def guessShift(cipherText: String): Option[Int] = {
        bestShifts(Histogram(cipherText)).find(x => decryptAndParse(cipherText, x).isDefined)
    }

    /**
     * Method to shift an upper-case English character by a given number of places.
     *
     * @param x     an
     * @param shift the number of places to shift
     * @return
     */
    def doShift(x: Char, shift: Int): Char = ((x.toInt + shift - A + radix) % radix + A).toChar

    private def decryptAndParse(cipherText: String, shift: Int) = {
        val plainText = CaesarCipher(shift).decrypt(cipherText)
        val z: Try[Seq[String]] = englishParser.parseEnglishWords(plainText.toString)
//        println(z)
        z.toOption
    }

    private val A = 'A'

    private val radix = 26

    private val englishParser = new EnglishParser
}
