package jca

import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, Resource, Sync}
import scala.annotation.tailrec
import scala.util.Random
import tsec.hashing.CryptoHash
import tsec.hashing.bouncy.Keccak256
import tsec.hashing.jca.*

trait Hashable[A, C] {
    def hash(x: Array[Byte]): IO[C]

    def bytes(c: C): Array[Byte]
}

/**
 * This class was originally contributed by Zhilue Wang (NiftyMule on github).
 */
trait MerkleTree[C] {
    def getHash: IO[C]
}

case class MerkleTreeNode[C, A](left: MerkleTree[C], right: MerkleTree[C])(implicit aCh: Hashable[A, C]) extends MerkleTree[C] {
    def getHash: IO[C] = for {
        leftHash <- left.getHash
        rightHash <- right.getHash
        hash <- aCh.hash(aCh.bytes(leftHash) ++ aCh.bytes(rightHash))
    } yield hash
}

case class MerkleTreeLeaf[C, A](content: String)(implicit aCh: Hashable[A, C]) extends MerkleTree[C] {
    def getHash: IO[C] = aCh.hash(content.getBytes)
}

object MerkleTree {
    trait CryptoHashSHA256Hashable extends Hashable[SHA256, CryptoHash[SHA256]] {
        def hash(x: Array[Byte]): IO[CryptoHash[SHA256]] = SHA256.hash[IO](x)

        def bytes(c: CryptoHash[SHA256]): Array[Byte] = c.bytes
    }

    implicit object CryptoHashSHA256Hashable extends CryptoHashSHA256Hashable

    def apply(seq: Seq[String]): MerkleTree[CryptoHash[SHA256]] = {
        @tailrec
        def inner(seq: Seq[MerkleTree[CryptoHash[SHA256]]]): MerkleTree[CryptoHash[SHA256]] = {
            val newSeq: Seq[MerkleTree[CryptoHash[SHA256]]] = seq.grouped(2).map(x => if x.length == 1 then x.head else MerkleTreeNode(x.head, x.last)).toSeq
            if newSeq.length > 1 then inner(newSeq) else newSeq.head
        }

        inner(seq.map(MerkleTreeLeaf.apply))
    }
}

object test extends App {
    val strings = Array(
        "When I was one-and-twenty",
        "I heard a wise man say",
        "Give crowns and pounds and guineas",
        "But not your heart away",
        "Give pearls away and rubies",
        "But keep your fancy free",
        "But I was one-and-twenty",
        "No use to talk to me",
        "When I was one-and-twenty",
        "I heard him say again",
        "The heart out of the bosom",
        "Was never given in vain",
        "Tis paid with sighs a plenty",
        "And sold for endless rue",
        "And I am two-and-twenty",
        "And oh tis true tis true"
    )
    val tree: MerkleTree[CryptoHash[SHA256]] = MerkleTree(strings)

    tree.getHash.unsafeRunSync().bytes.foreach(x => print("%02X".format(x)))
    println()


    // block chain assignment
    val transactions = LazyList.continually(Seq(
        "00000dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
        "Harry pays Robin 1.000",
        "Maharshi pays Harry 1.000",
        "Pranshu pays Maharshi 1.000",
        "Robin pays Pranshu 1.000"
    ))

    val start = System.nanoTime()
//  Random.setSeed(22)
    transactions
            .map(list => list :+ Random.nextBytes(5).foldLeft("")((z: String, x: Byte) => z + "%02X".format(x))) // append nonce
            .find(x => {
                val hashBytes = MerkleTree(x).getHash.unsafeRunSync().bytes
                if hashBytes(0) == 0 && hashBytes(1) == 0 && "%02X".format(hashBytes(2)).startsWith("0") then {
                    hashBytes.foreach(x => print("%02X".format(x)))
                    println()
                    println(MerkleTree(x))
                    true
                } else false
            })
            .foreach(println)
    val end = System.nanoTime()
    val elapsed = end - start
    println("duration: " + elapsed + "ns")

    // validate
    val tree2: MerkleTree[CryptoHash[SHA256]] = MerkleTree(transactions.head :+ "C188EB411C")
    tree2.getHash.unsafeRunSync().bytes.foreach(x => print("%02X".format(x)))
    println()
}