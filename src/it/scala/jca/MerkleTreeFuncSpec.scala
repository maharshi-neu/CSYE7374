package jca

import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, Resource, Sync}
import jca.MerkleTree.Bytes
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
        Random.setSeed(22)

        val transactions = Seq(
            "Harry pays Robin 1.000",
            "Maharshi pays Harry 1.000",
            "Pranshu pays Maharshi 1.000",
            "Robin pays Pranshu 1.000"
        )
        val priorBlockHash = "00000dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824".getBytes()

        val (nonce, hash) = MerkleTree.mineMerkleTreeBlock(transactions, priorBlockHash)

//        println(s"${util.Hex.bytesToHexString(nonce)}, ${util.Hex.bytesToHexString(hash)}")

        util.Hex.bytesToHexString(nonce) shouldBe "62BA1849C6"
        util.Hex.bytesToHexString(hash) shouldBe "00000D7094FE96F3CAD81756320A3B135624BCF2CD22BC07017D69503B79A8DF"


        // validate
//        val tree2: MerkleTree[CryptoHash[SHA256]] = MerkleTree(lazyList.head :+ "C188EB411C")
//        val q = for (h <- tree2.getHash; z <- HexEncryption.bytesToHexString(h)) yield z
//        q.unsafeRunSync() shouldBe "0000057620DDBA668BB0945755C86A5A403706D19EE2A6869E837657607C7428"

    }

}
