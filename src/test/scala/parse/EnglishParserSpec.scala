package parse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EnglishParserSpec extends AnyFlatSpec with should.Matchers {

    val parser = new EnglishParser

    behavior of "EnglishParser"

    it should "word2" in {
        parser.parseAll(parser.word2, "to") should matchPattern { case parser.Success(_) => }
        parser.parseAll(parser.word2, "12").successful shouldBe false
    }

    it should "wordParser1" in {
        parser.parseAll(parser.wordParser, "a").successful shouldBe true
        parser.parseAll(parser.wordParser, "hello").successful shouldBe true
        parser.parseAll(parser.wordParser, "world").successful shouldBe true
        parser.parseAll(parser.wordParser, "helloworld").successful shouldBe false
    }

    it should "wordParser2" in {
        parser.parseAll(parser.word3, "the") should matchPattern { case parser.Success("the") => }
        parser.parseAll(parser.word6, "rapist") should matchPattern { case parser.Success("rapist") => }
        parser.parseAll(parser.word9, "therapist") should matchPattern { case parser.Success("therapist") => }
        parser.parseAll(parser.wordParser, "therapist") should matchPattern { case parser.Success("therapist") => }
    }

    it should "word9" in {
        parser.parseAll(parser.word9, "facetious") should matchPattern { case parser.Success(_) => }
    }

    it should "wordsParser" in {
        parser.parseAll(parser.wordsParser, "helloworld").successful shouldBe true

    }

    it should "parseEnglishWords" in {
        parser.parseEnglishWords("helloworld") should matchPattern { case scala.util.Success(Seq("hello", "world")) => }
        parser.parseEnglishWords("hello world") should matchPattern { case scala.util.Success(Seq("hello", "world")) => }
    }

}
