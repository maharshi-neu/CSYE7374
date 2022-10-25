import java.security.Provider
import java.security.Security.{addProvider, getProvider}
import org.bouncycastle.crypto.digests.SHA256Digest
import util.Hex

val sha256 = new SHA256Digest()

val helloworld = "hello"

val retValue = new Array[Byte](sha256.getDigestSize)

sha256.update(helloworld.getBytes(), 0, helloworld.length)
sha256.doFinal(retValue, 0)

retValue

def byte2hex(buff: Array[Byte]): String =
    buff.map(x => "%02x" format x).mkString

byte2hex(retValue)

val z = Hex.bytesToHexString(retValue)

import org.bouncycastle.jce.provider.BouncyCastleProvider

addProvider(new BouncyCastleProvider)
val provider = getProvider("BC")
