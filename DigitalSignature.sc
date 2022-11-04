import java.security.{SecureRandom}
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.engines.RSAEngine
import org.bouncycastle.crypto.params.AsymmetricKeyParameter
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.generators.RSAKeyPairGenerator
import org.bouncycastle.crypto.params.RSAKeyGenerationParameters
import java.math.BigInteger
import java.nio.file.{Files, Paths}

def createPair(): AsymmetricCipherKeyPair = {
  val generator = new RSAKeyPairGenerator()
  generator.init(new RSAKeyGenerationParameters(
    BigInteger("1001", 16),
    SecureRandom.getInstance("SHA1PRNG"),
    4096,
    80
  ))
  generator.generateKeyPair()
}

def encrypt(data: Array[Byte], key: AsymmetricKeyParameter): Array[Byte] = {
  val engine = new RSAEngine()
  engine.init(true, key)
  engine.processBlock(data, 0, data.length)
}

def decrypt(data: Array[Byte], key: AsymmetricKeyParameter): Array[Byte] = {
  val engine = new RSAEngine()
  engine.init(false, key)
  engine.processBlock(data, 0, data.length)
}

def getSHA(data: Array[Byte]): Array[Byte] = {
  val sha256 = new SHA256Digest()
  sha256.update(data, 0, data.length)
  val hval = new Array[Byte](sha256.getDigestSize)
  sha256.doFinal(hval, 0)
  hval
}

def signer(data: Array[Byte], key: AsymmetricKeyParameter): Array[Byte] = {
  val hval = getSHA(data)
  encrypt(hval, key)
}

def verifier(data: Array[Byte], signature: Array[Byte], key: AsymmetricKeyParameter): Boolean = {
  val hval = getSHA(data)
  val dval = decrypt(signature, key)

  dval.sameElements(hval)
}

val pair: AsymmetricCipherKeyPair = createPair()
val privateKey: AsymmetricKeyParameter = pair.getPrivate()
val publicKey: AsymmetricKeyParameter = pair.getPublic()

val f = Files.readAllBytes(Paths.get("C:/work/csye7374/CSYE7374/build.sbt"))

val signature = signer(f, privateKey)
verifier(f, signature, publicKey)
