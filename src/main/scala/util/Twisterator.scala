package util

/**
 * This class defines takes an iterator based on a given equivalent iterator, except for the "twist in the tail."
 *
 * @param twist    a function which is applied to the last element of the iterator.
 * @param iterator the input iterator.
 * @tparam X the underlying type.
 * @return a new Iterator which a twisted tail.
 */
case class Twisterator[X](twist: X => X)(iterator: Iterator[X]) extends Iterator[X] {
    def hasNext: Boolean = iterator.hasNext

    def next(): X =
        val x = iterator.next()
        if iterator.hasNext then x else twist(x)
}
