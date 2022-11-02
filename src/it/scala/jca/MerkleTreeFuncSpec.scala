package jca

import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, Resource, Sync}
import jca.test.tree
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.annotation.tailrec
import scala.util.Random
import tsec.hashing.CryptoHash
import tsec.hashing.bouncy.Keccak256
import tsec.hashing.jca.*

class MerkleTreeFuncSpec extends AnyFlatSpec with should.Matchers {

    behavior of "MerkleTreeFunc"

    it should "apply" in {

        val nonce = "00000dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"

        val transactions = Seq(
            "Harry pays Robin 1.000",
            "Maharshi pays Harry 1.000",
            "Pranshu pays Maharshi 1.000",
            "Robin pays Pranshu 1.000"
        )

        // block chain assignment
        val lazyList = LazyList.continually(nonce +: transactions)

        Random.setSeed(22)
        lazyList.
                map(list => list :+ Random.nextBytes(5).foldLeft("")((z: String, x: Byte) => z + "%02X".format(x))). // append nonce
                find(x => {
                    val hashBytes = MerkleTree(x).getHash.unsafeRunSync().bytes
                    if hashBytes(0) == 0 && hashBytes(1) == 0 && "%02X".format(hashBytes(2)).startsWith("0") then {
                        util.Hex.bytesToHexString(hashBytes) shouldBe "0000057620DDBA668BB0945755C86A5A403706D19EE2A6869E837657607C7428"
                        println(MerkleTree(x))
                        true
                    } else false
                })
                .foreach(println)

        // validate
        val tree2: MerkleTree[CryptoHash[SHA256]] = MerkleTree(lazyList.head :+ "C188EB411C")
        val q = for (h <- tree2.getHash; z <- HexEncryption.bytesToHexString(h)) yield z
        q.unsafeRunSync() shouldBe "0000057620DDBA668BB0945755C86A5A403706D19EE2A6869E837657607C7428"

    }

}
