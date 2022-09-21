package crypto

import crypto.CaesarCipher.doShift
import scala.annotation.unused

case class Histogram(map: Map[Char, Int]) {
    /**
     * Method to record instance of this character in this Histogram.
     *
     * @param x the character to record.
     * @return a new Histogram.
     */
    def recordChar(x: Char): Histogram = Histogram(map + (x -> (map.getOrElse(x, 0) + 1)))

    /**
     * Method to "rotate" this Histogram such that each character is transformed into another character.
     *
     * @param f the transformation function.
     * @return a new Histogram.
     */
    def rotate(f: Char => Char): Histogram = Histogram(for ((x, count) <- map) yield f(x) -> count)

    /**
     * Method to yield that sum of the values in this Histogram.
     *
     * @return the total.
     */
    lazy val total: Int = map.values.sum

    /**
     * Method to compare this Histogram with another.
     *
     * @param h the Histogram with which to compare this.
     * @return a measure of the difference between this and h.
     */
    def comparer(h: Histogram): Double = {
        val thisTotal: Double = this.total
        val thatTotal: Double = h.total
        val diff = for ((y, z) <- map.values zip h.map.values; q = y / thisTotal - z / thatTotal) yield q * q
        diff.sum
    }

    /**
     * Method to get the number of usages of the given key character as an Option[Int].
     *
     * @param key the character.
     * @return an optional integer value.
     */
    def get(key: Char): Option[Int] = map.get(key)

    /**
     * Method to get the number of usages of the given key character, with a default value.
     *
     * @param key     the character.
     * @param default the value to be returned if key is not present in this Histogram.
     * @tparam X the type of the value which will be returned: a supertype of Int.
     * @return a value of type X.
     */
    def getOrElse[X >: Int](key: Char, default: => X): X = map.getOrElse(key, default)

    /**
     * Method to get all the characters from this Histogram.
     *
     * @return an Iterable of Char.
     */
    def keys: Iterable[Char] = map.keys

    @unused
    /**
     * Method to get all the count values from this Histogram.
     *
     * @return an Iterable of Int.
     */
    def values: Iterable[Int] = map.values

    /**
     * Method to determine if key is present in this Histogram.
     *
     * @param key the character.
     * @return true if present, otherwise false.
     */
    def contains(key: Char): Boolean = map.contains(key)

    /**
     * Method to get the characters as a Set.
     *
     * @return a Set[Char].
     */
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
