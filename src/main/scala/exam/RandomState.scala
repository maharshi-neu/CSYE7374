package exam

import scala.util.Random


/**
 * Monadic trait which defines a random-state.
 *
 * Created by scalaprof on 9/24/16.
 *
 * @tparam T the underlying type of this random state, i.e. the type of the result of calling get
 */
trait RandomState[T] {
    /**
     * @return the next random state in the pseudo-random series
     */
    def next: RandomState[T]

    /**
     * @return the value of this random state
     */
    def get: T

    /**
     * Method to map this random state into another random state
     *
     * @param f the function to map a T value into a U value
     * @tparam U the underlying type of the resulting random state
     * @return a new random state
     */
    def map[U](f: T => U): RandomState[U]

    /**
     * Method to flatMap this random state into another random state
     *
     * @param f the function to map a T value into a RandomState[U] value
     * @tparam U the underlying type of the resulting random state
     * @return a new random state
     */
    // Hint: Think of the input and output, find the appropriate method that achieve this.
    // 10 points
    def flatMap[U](f: T => RandomState[U]): RandomState[U] = /*SOLUTION*/ f(get) /*END*/

    /**
     * @return a stream of T values
     */
    // Hint: This a recursively method and it concatenate current element with following elements.
    // 12 points
    def toStream: LazyList[T] = /*SOLUTION*/ LazyList.cons[T](get, next.toStream) /*END*/
}

/**
 * A concrete implementation of RandomState based on the Java random number generator
 *
 * @param n the random Long that characterizes this random state
 * @param g the function which maps a Long value into a T
 * @tparam T the underlying type of this random state, i.e. the type of the result of calling get
 */
case class JavaRandomState[T](n: Long, g: Long => T) extends RandomState[T] {
    // Hint: Remember to use the "seed" to generate next RandomState.
    // 7 points
    def next: RandomState[T] = /*SOLUTION*/ JavaRandomState[T](new Random(n).nextLong(), g)

    /*END*/
    // Hint: Think of the input and output.
    // 5 points
    def get: T = /*SOLUTION*/ g(n)

    /*END*/
    // Hint: This one need function composition.
    // 13 points
    def map[U](f: T => U): RandomState[U] = /*SOLUTION*/ JavaRandomState[U](n, g andThen f) /*END*/
}

object JavaRandomState {
    def apply[T](g: Long => T) = new JavaRandomState[T](System.currentTimeMillis(), g)
}
