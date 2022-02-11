package crypto

case class Composite(factors: Seq[Prime]) {
  def ady: Int = factors.size
}

object Composite {
  def apply(x: BigInt): Composite = Composite(Prime.primeFactors(x))
}
