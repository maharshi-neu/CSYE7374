package util

/**
 * This class defines takes an iterator based on a given equivalent iterator, except for the "twist in the tail."
 *
 * @param f        a function which is applied to all elements of the iterator except the last.
 * @param twist    a function which is applied to the last element of the iterator.
 * @param iterator the input iterator.
 * @tparam X the underlying type of the input iterator.
 * @tparam Y the underlying type of the output iterator.
 * @return a new Iterator[X] which a twisted tail.
 */
case class Twisterator[X, Y](f: X => Y, twist: X => Y)(iterator: Iterator[X]) extends Iterator[Y] {
    def hasNext: Boolean = iterator.hasNext

    def next(): Y =
        val x = iterator.next()
        if iterator.hasNext then f(x) else twist(x)
}

object Twisterator {
    /**
     * Method to construct a Twisterator with only one parametric type and one function.
     *
     * @param twist    a function of type X => X.
     * @param iterator an Iterator[X].
     * @tparam X the underlying type the result and iterator.
     * @return a Twisterator[X,X] which is also an Iterator[X].
     */
    def apply[X](twist: X => X)(iterator: Iterator[X]): Twisterator[X, X] = new Twisterator[X, X](identity, twist)(iterator)
}
