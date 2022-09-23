import cats.Id
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, Resource, Sync}
import fs2.*
import java.util.function.Consumer
import scala.concurrent.duration.*
import tsec.common.*
import tsec.hashing.CryptoHash
import tsec.hashing.jca.*

val hashIo: IO[CryptoHash[SHA256]] = SHA256.hash[IO]("hiHello".utf8Bytes)
val result: CryptoHash[SHA256] = hashIo.unsafeRunSync()
for (byte <- result.bytes) println(byte)
