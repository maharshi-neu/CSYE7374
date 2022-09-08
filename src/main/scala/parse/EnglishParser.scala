package parse

import crypto.CryptoTools
import parse.EnglishParser.toLowerCase
import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

class EnglishParser extends JavaTokenParsers {

    /**
     * Method to parse a sequence of English words as a Try of a list of words.
     * The method is designed especially for testing guessed plain text after decryption.
     *
     * NOTE: see comment re: wordParser
     *
     * @param w the string to be parsed--it may contain spaces and/or a mix of any case.
     * @return Success(list) if all words are English; otherwise Failure if there are any non-English words, digits or punctuation (other than space).
     */
    def parseEnglishWords(w: CharSequence): Try[Seq[String]] = parseAll(wordsParser, toLowerCase(w)) match {
        case Success(ws, _) => scala.util.Success(ws)
        case Failure(x, z) => scala.util.Failure(EnglishParserException(s"$x: $z"))
        case Error(x, z) => scala.util.Failure(EnglishParserException(s"$x: $z"))
    }

    def wordsParser: Parser[Seq[String]] = rep(wordParser)

    // NOTE: this does not work perfectly. In the PoohIntroduction2 text, "as far as he knows" is matched as "as far ash" followed by an error.
    def wordParser: Parser[String] = wordMany ||| word15 ||| word14 ||| word13 ||| word12 ||| word11 ||| word10 ||| word9 ||| word8 ||| word7 ||| word6 ||| word5 ||| word4 ||| word3 ||| word2 ||| word1 ||| failure("not English word")

    private def englishWordParser(r: Regex): Parser[String] = regex(r).filter { w => CryptoTools.isEnglishWordExactCase(w) }

    def word1: Parser[String] = englishWordParser("""[ai]""".r)

    def word2: Parser[String] = englishWordParser("""\w{2}""".r)

    def word3: Parser[String] = englishWordParser("""\w{3}""".r)

    def word4: Parser[String] = englishWordParser("""\w{4}""".r)

    def word5: Parser[String] = englishWordParser("""\w{5}""".r)

    def word6: Parser[String] = englishWordParser("""\w{6}""".r)

    def word7: Parser[String] = englishWordParser("""\w{7}""".r)

    def word8: Parser[String] = englishWordParser("""\w{8}""".r)

    def word9: Parser[String] = englishWordParser("""\w{9}""".r)

    def word10: Parser[String] = englishWordParser("""\w{10}""".r)

    def word11: Parser[String] = englishWordParser("""\w{11}""".r)

    def word12: Parser[String] = englishWordParser("""\w{12}""".r)

    def word13: Parser[String] = englishWordParser("""\w{13}""".r)

    def word14: Parser[String] = englishWordParser("""\w{14}""".r)

    def word15: Parser[String] = englishWordParser("""\w{15}""".r)

    def wordMany: Parser[String] = englishWordParser("""\w{15}\w+""".r)

}

object EnglishParser {
    def toLowerCase(w: CharSequence): CharSequence = w.toString.toLowerCase
}

case class EnglishParserException(s: String) extends Exception(s)

