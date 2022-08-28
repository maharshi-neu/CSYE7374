package parse

import crypto.CryptoTools
import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

class EnglishParser extends JavaTokenParsers {

    def parseEnglishWords(w: String): Try[Seq[String]] = parseAll(wordsParser, w) match {
        case Success(ws, _) => scala.util.Success(ws)
        case Failure(x, z) => scala.util.Failure(EnglishParserException(s"$x: $z"))
        case Error(x, z) => scala.util.Failure(EnglishParserException(s"$x: $z"))
    }

    def wordsParser: Parser[Seq[String]] = rep(wordParser)

    def wordParser: Parser[String] = wordMany | word15 | word14 | word13 | word12 | word11 | word10 | word9 | word8 | word7 | word6 | word5 | word4 | word3 | word2 | word1

    private def englishWordParser(r: String): Parser[String] = regex(r.r).filter { w => CryptoTools.isEnglishWord(w) }

    def word1: Parser[String] = englishWordParser("""\w""")

    def word2: Parser[String] = englishWordParser("""\w{2}""")

    def word3: Parser[String] = englishWordParser("""\w{3}""")

    def word4: Parser[String] = englishWordParser("""\w{4}""")

    def word5: Parser[String] = englishWordParser("""\w{5}""")

    def word6: Parser[String] = englishWordParser("""\w{6}""")

    def word7: Parser[String] = englishWordParser("""\w{7}""")

    def word8: Parser[String] = englishWordParser("""\w{8}""")

    def word9: Parser[String] = englishWordParser("""\w{9}""")

    def word10: Parser[String] = englishWordParser("""\w{10}""")

    def word11: Parser[String] = englishWordParser("""\w{11}""")

    def word12: Parser[String] = englishWordParser("""\w{12}""")

    def word13: Parser[String] = englishWordParser("""\w{13}""")

    def word14: Parser[String] = englishWordParser("""\w{14}""")

    def word15: Parser[String] = englishWordParser("""\w{15}""")

    def wordMany: Parser[String] = englishWordParser("""\w{15}\w+""")

}

case class EnglishParserException(s: String) extends Exception(s)

