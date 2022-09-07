package crypto

import crypto.CaesarCipher.doShift
import scala.annotation.unused

case class Histogram(map: Map[Char, Int]) {
    def recordChar(x: Char): Histogram = Histogram(map + (x -> (map.getOrElse(x, 0) + 1)))

    def rotate(f: Char => Char): Histogram = Histogram(for ((x, y) <- map) yield f(x) -> y)

    def total: Int = map.values.sum

    def comparer(h: Histogram): Double = {
        val thisTotal: Double = this.total
        val thatTotal: Double = h.total
        val diff = for ((y, z) <- map.values zip h.map.values; q = y / thisTotal - z / thatTotal) yield q * q
        diff.sum
    }

    def get(key: Char): Option[Int] = map.get(key)

    def getOrElse[V1 >: Int](key: Char, default: => V1): V1 = map.getOrElse(key, default)

    def keys: Iterable[Char] = map.keys

    def values: Iterable[Int] = map.values

    def contains(key: Char): Boolean = map.contains(key)

    def keySet: Set[Char] = map.keySet
}

object Histogram {

    /**
     * Method to construct a Histogram from a given string.
     *
     * @param w the string (typically ciphertext). Must be in upper case.
     * @return the Histogram for the string w.
     */
    def apply(w: CharSequence): Histogram = (for (x <- w.toString) yield x).foldLeft(Histogram(Map())) { (m, x) => m.recordChar(x) }

    /**
     * The English Histogram.
     * NOTE: from https://en.wikipedia.org/wiki/Letter_frequency
     */
    val English: Histogram = Histogram(Map(
        'A' -> 82, 'B' -> 15, 'C' -> 28, 'D' -> 43, 'E' -> 130,
        'F' -> 22, 'G' -> 20, 'H' -> 61, 'I' -> 70, 'J' -> 1,
        'K' -> 8, 'L' -> 40, 'M' -> 24, 'N' -> 67, 'O' -> 75,
        'P' -> 19, 'Q' -> 1, 'R' -> 60, 'S' -> 63, 'T' -> 91,
        'U' -> 28, 'V' -> 10, 'W' -> 24, 'X' -> 1, 'Y' -> 20, 'Z' -> 1
    ))

    def shiftedEnglishHistograms: Seq[(Histogram, Int)] =
        (for (i <- 0 to 25) yield English.rotate(x => doShift(x, i))).zipWithIndex

}
