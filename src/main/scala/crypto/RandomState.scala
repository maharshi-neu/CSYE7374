package crypto

import java.math.BigInteger
import scala.util.Random

class RandomState(r: Random) {
  def value(bits: Int): BigInt = new BigInteger(bits, r.self)

  def value(x: BigInt): BigInt = value(x.bitLength) % x

  def next: RandomState = RandomState(r.nextLong())

  def bytes(n: Int): Array[Byte] = r.nextBytes(n)

  def lazyList: LazyList[RandomState] = LazyList.iterate(this)(r => r.next)
}

object RandomState {
  def apply(seed: Long): RandomState = new RandomState(new Random(seed))

  def lazyList(seed: Long): LazyList[RandomState] = {
    def z(r: RandomState): LazyList[RandomState] = r #:: z(r.next)

    z(apply(seed))
  }
}
