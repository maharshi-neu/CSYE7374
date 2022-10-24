package exam

import exam.Exam.{hundredPrimes, likelyPrime, satisfiesFermatsTest}
import exam.RandomState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Random

class ExamSpec extends AnyFlatSpec with should.Matchers {

    behavior of "Exam"

    it should "gcd" in {
        Exam.gcd(10, 2) shouldBe 2L
        Exam.gcd(7, 5) shouldBe 1L
        Exam.gcd(5, 7) shouldBe 1L
    }

    it should "hundredPrimes.count" in {
        hundredPrimes.count(_ < 100) shouldBe 25
        hundredPrimes.count(_ < 200) shouldBe 46
        hundredPrimes.count(_ < 500) shouldBe 95
        hundredPrimes.count(_ < 1000) shouldBe 100
    }
    it should "stream" in {
        val m = 5
        val n: BigInt = 541
        val nPrimes = hundredPrimes.count(_ < n)
        val randomState: JavaRandomState[BigInt] = JavaRandomState(0L, BigInt(_) - Long.MinValue)
        val candidates: Seq[BigInt] = randomState.toStream.map(x => x % nPrimes).take(m).to(List).map(x => hundredPrimes(x.toInt))
        candidates shouldBe List(23, 397, 41, 17, 113)
    }
    it should "satisfiesFermatTests" in {
        val m = 5
        val n: BigInt = 541
        val nPrimes = hundredPrimes.count(_ < n)
        val randomState: JavaRandomState[BigInt] = JavaRandomState(0L, BigInt(_) - Long.MinValue)
        val candidates: Seq[BigInt] = randomState.toStream.map(x => x % nPrimes).take(m).to(List).map(x => hundredPrimes(x.toInt))
        candidates.forall(a => satisfiesFermatsTest(a, n)) shouldBe true
    }
    it should "likelyPrime" in {
        likelyPrime(397) shouldBe true
    }

}
