package crypto

import scala.util.{Try, Using}

object CryptoTools {

    private val dictionaryFile = "/usr/share/dict/web2"
    // NOTE: this is only expected to work for Macs.
    val triedDictionary: Try[Set[String]] = getDictionary(dictionaryFile)

    private def getDictionary(file: String): Try[Set[String]] = Using(scala.io.Source.fromFile(file)) { s => s.getLines.toSet }

    def isEnglishWord(w: String): Boolean = triedDictionary.map(d => d.contains(w.toLowerCase)).getOrElse(false)

    def parseEnglish(w: String): Option[Int] = {
        val min2 = math.min(w.length, 15)
        val min1 = math.min(w.length, 3)
        val result: Seq[Int] = for (prefixLength <- min1 to min2; if isEnglishWord(w.substring(0, prefixLength))) yield prefixLength
        result.headOption
    }
}
