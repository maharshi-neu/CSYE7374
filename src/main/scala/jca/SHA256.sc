import cats.Id
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, Resource, Sync}
import fs2.*
import java.util.function.Consumer
import scala.concurrent.duration.*
import tsec.common.*
import tsec.hashing.CryptoHash
import tsec.hashing.bouncy.Keccak256
import tsec.hashing.jca.*

val hashIoJCA: IO[CryptoHash[SHA256]] = SHA256.hash[IO]("hiHello".utf8Bytes)
val resultJCA: CryptoHash[SHA256] = hashIoJCA.unsafeRunSync()
for (byte <- resultJCA.bytes) println(byte)

val hashIoBC: IO[CryptoHash[Keccak256]] = tsec.hashing.bouncy.Keccak256.hash[IO]("hiHello".utf8Bytes)
val resultBC: CryptoHash[Keccak256] = hashIoBC.unsafeRunSync()
for (byte <- resultBC.bytes) println(byte)

