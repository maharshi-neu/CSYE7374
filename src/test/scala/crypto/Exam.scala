package crypto

object Exam {

  case class Point(a: Int, b: Int) {
    def move1: Point = Point(a + b, b)

    def move2: Point = Point(a, a + b)
  }

  object Point {
    def parse(u: String, w: String): Option[Point] = for (x <- u.toIntOption; y <- w.toIntOption) yield Point(x, y)
  }

  /*
   * Complete the 'isPossible' function below.
   *
   * The function is expected to return a STRING.
   * The function accepts following parameters:
   *  1. String a
   *  2. String b
   *  3. String c
   *  4. String d
   */

  def isPossible(a: String, b: String, c: String, d: String): String = {

    def inRange(s: Point, t: Point): Boolean = s.a <= t.a && s.b <= t.b

    def reach(s: Point, t: Point): Boolean = s == t || (inRange(s, t) && (reach(s.move1, t) || reach(s.move2, t)))

    (for (s <- Point.parse(a, b); t <- Point.parse(c, d)) yield reach(s, t)) match {
      case None => "Invalid"
      case Some(true) => "Yes"
      case Some(false) => "No"
    }
  }
}


import crypto.Exam.isPossible
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.language.postfixOps

class ExamSpec extends AnyFlatSpec with should.Matchers {
  behavior of "Exam"

  it should "work" in {
    isPossible("1", "1", "1", "1") shouldBe "Yes"
    isPossible("1", "1", "1", "2") shouldBe "Yes"
    isPossible("1", "2", "3", "2") shouldBe "Yes"
    isPossible("3", "2", "5", "2") shouldBe "Yes"
    isPossible("1", "4", "5", "9") shouldBe "Yes"
    isPossible("1", "2", "3", "6") shouldBe "No"
    isPossible("2", "2", "2", "1000") shouldBe "Yes"
    isPossible("1", "4", "62", "45") shouldBe "Yes"
    isPossible("8", "9", "8", "9") shouldBe "Yes"
    isPossible("1", "2", "2", "1") shouldBe "No"
    isPossible("1", "1", "1000", "1000") shouldBe "No"
    isPossible("1", "1", "10", "12") shouldBe "No"
    isPossible("789", "789", "1", "1000") shouldBe "No"
    isPossible("2", "2", "1000", "998") shouldBe "Yes"
  }

  it should "fail" in {
    isPossible("A", "B", "C", "D") shouldBe "Invalid"
  }
}

