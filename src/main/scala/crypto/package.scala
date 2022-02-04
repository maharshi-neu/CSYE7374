package object crypto {
  /**
   * Implicit class to determine if a number (x) is a factor of another number (y).
   * NOTE that for potential factors larger than an Int, you should just use % directly.
   *
   * @param x an Int.
   */
  implicit class Divides(x: Int) {
    /**
     * Method to test if x divides y.
     *
     * @param y an Int.
     * @return true if x divides y.toLong.
     */
    def |(y: Int): Boolean = |(y.toLong)

    /**
     * Method to test if x divides y.
     *
     * @param y a Long.
     * @return true if x divides y.
     */
    def |(y: Long): Boolean = y % x == 0

    /**
     * Method to test if x divides y.
     *
     * @param y a BigInt.
     * @return true if x divides y.
     */
    def |(y: BigInt): Boolean = y % x == 0
  }
}
