import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import util.Benchmark

import java.math.BigInteger
import java.util.Random
import java.util.function.{Consumer, Supplier}

val random = new Random()
val numBits = 160
val primeSupplier: Supplier[BigInteger] = new Supplier[BigInteger]() {
    def get(): BigInteger = new BigInteger(numBits, random)
}
val certainty = 40
val runs = 10000
val primeTester: Consumer[BigInteger] = t => {
    val _ = t.isProbablePrime(certainty)
}
val benchmark: Benchmark[BigInteger] = new Benchmark[BigInteger]("isProbablePrime BigInt", null, primeTester)
benchmark.run(primeSupplier, runs)
