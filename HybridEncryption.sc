import java.nio.charset.Charset
import java.io.FileReader
import org.bouncycastle.openssl.PEMParser
import java.nio.file.{Files, Paths}
import org.bouncycastle.openssl.jcajce.JcaPEMKeyConverter
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
import java.security.{PublicKey, PrivateKey}
import javax.crypto.Cipher
import java.security.SecureRandom
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import org.bouncycastle.openssl.PEMKeyPair

def getPublicKeyFromFile(path: String): PublicKey = {
  val pp = PEMParser(FileReader(path));
  val converter = new JcaPEMKeyConverter()
  val publicKeyInfo = SubjectPublicKeyInfo.getInstance(pp.readObject())
  converter.getPublicKey(publicKeyInfo)
}

def getPrivateKeyFromFile(path: String): PrivateKey = {
  val pp = PEMParser(FileReader(path));
  val converter = new JcaPEMKeyConverter()
  val kp = converter.getKeyPair(pp.readObject().asInstanceOf[PEMKeyPair])
  kp.getPrivate()
}

def encryptRSA(data: Array[Byte], key: PublicKey): Array[Byte] = {
  val cipher = Cipher.getInstance("RSA")
  cipher.init(Cipher.ENCRYPT_MODE, key)
  cipher.doFinal(data)
}

def decryptRSA(data: Array[Byte], key: PrivateKey): Array[Byte] = {
  val cipher = Cipher.getInstance("RSA")
  cipher.init(Cipher.DECRYPT_MODE, key)
  cipher.doFinal(data)
}

def getRandomBytes(size: Int): Array[Byte] = {
  val bytes = Array.ofDim[Byte](size)
  val rnd = new SecureRandom()
  rnd.nextBytes(bytes)
  bytes
}

class AES128(key: Array[Byte]) {
  val _iv = new IvParameterSpec(Array.fill(128 / 8) { 0.toByte })
  val skeySpec = new SecretKeySpec(key, "AES")

  def encrypt(data: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec, _iv)
    cipher.doFinal(data)
  }

  def decrypt(chipher: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
    cipher.init(Cipher.DECRYPT_MODE, skeySpec, _iv)
    cipher.doFinal(chipher)
  }
}

def byte2hex(buff: Array[Byte]): String =
  buff.map(x => "%02X" format x).mkString

def hex2string(hex: IndexedSeq[String]): String =
  hex.map(Integer.parseInt(_, 16).toChar).mkString

// customize this prefixpath accoring to your system
val prefixpath = "C:\\work\\csye7374\\inclass\\"

val profpem = prefixpath + "ProfRobinsPublicKey.pem"
val mepri = prefixpath + "maharshi_private.pem"
val mepub = prefixpath + "maharshi_public-key.pem"
//val key = String(Files.readAllBytes(Paths.get(profpem)), Charset.defaultCharset())

val message: String =
  """
    |Chess is an ancient strategy game that originated in India.
    |It is played by two individuals on an 8×8 grid.
    |The objective is to maneuver one's pieces so as to trap
    |the opposing king in "checkmate". This book will cover
    |the basic pieces of chess, before going on to some more advanced topics.
    |
    |The history of chess began in India during the Gupta Empire,
    |where its early form in the 6th century CE was
    |known as chaturanga, which translates as
    |"four divisions of the military" – infantry, cavalry, elephants,
    |and chariotry, represented by the pieces that would evolve
    |into the modern pawn, knight, bishop, and rook, respectively.
    |
    |In Sassanid Persia around 600 CE, the name became shatranj,
    |and the rules were developed further. Shatranj was taken up by
    |the Muslim world after the Islamic conquest of Persia, with
    |the pieces largely retaining their Persian names.
    |In Spanish, "shatranj" was rendered as ajedrez, in
    |Portuguese as xadrez, and in Greek as zatrikion, but in the
    |rest of Europe it was replaced by versions of the Persian shāh ("king").
    |""".stripMargin

val pubKey = getPublicKeyFromFile(profpem)

val key = getRandomBytes(128 / 8)

val aes = new AES128(key)
val cipher = aes.encrypt(message.getBytes())

val encryptedKey = encryptRSA(key, pubKey)

val encryptedKeyPath = prefixpath + "encryptedKey_RSA"
val encryptedDataPath = prefixpath + "encryptedData_AES_0IV"
Files.write(Paths.get(encryptedDataPath), cipher)
Files.write(Paths.get(encryptedKeyPath), encryptedKey)

// OTHER SIDE (uncomment and plug in your private key
/*
val cipherTextFilePath = prefixpath + "encryptedData_AES_0IV"
val cipherKeyFilePath = prefixpath + "encryptedKey_RSA"

val cipherKey = Files.readAllBytes(Paths.get(cipherKeyFilePath))
val cipherText = Files.readAllBytes(Paths.get(cipherTextFilePath))

val priKey = getPrivateKeyFromFile(mepri) // enter your private key path here
val plainTextKey = decryptRSA(cipherKey, priKey)

val _aes = new AES128(plainTextKey)
val plainText = _aes.decrypt(cipherText)
String(plainText)
*/