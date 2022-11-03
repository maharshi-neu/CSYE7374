package jca

import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, Resource, Sync}
import jca.MerkleTree.Bytes
import scala.annotation.tailrec
import scala.util.Random
import tsec.hashing.CryptoHash
import tsec.hashing.bouncy.Keccak256
import tsec.hashing.jca.*

/**
 * This class was originally contributed by Zhilue Wang (NiftyMule on github).
 *
 * @tparam H the underlying hash type.
 */
trait MerkleTree[H] {
    /**
     * Method to get the hash of this Merkle Tree.
     *
     * @return an IO[H]
     */
    def getHash: IO[H]
}

/**
 * Case class to define an inner node of a MerkleTree.
 *
 * @param left         the left sub-tree.
 * @param right        the right sub-tree.
 * @tparam H the underlying hash type.
 */
case class MerkleTreeNode[H: Hashable](left: MerkleTree[H], right: MerkleTree[H]) extends MerkleTree[H] {
    private val hh = implicitly[Hashable[H]]

    def getHash: IO[H] = for {
        leftHash <- left.getHash
        rightHash <- right.getHash
        hash <- hh.hash(hh.bytes(leftHash) ++ hh.bytes(rightHash))
    } yield hash
}

/**
 * Case class to define a leaf node of a MerkleTree.
 *
 * @param content      the content of the leaf (an array of Byte).
 * @tparam H the underlying hash type.
 */
case class MerkleTreeLeaf[H: Hashable](content: Array[Byte]) extends MerkleTree[H] {
    def getHash: IO[H] = implicitly[Hashable[H]].hash(content)
}

object MerkleTreeLeaf {
    /**
     * Method to create a MerkleTreeLeaf with String content.
     *
     * @param content      a String.
     * @tparam H the underlying hash type.
     * @return a MerkleTreeLeaf[H].
     */
    def apply[H: Hashable](content: String): MerkleTreeLeaf[H] = apply(content.getBytes)
}

object MerkleTree {
    type Bytes = Array[Byte]

    def apply[H: Hashable](seq: Seq[Bytes]): MerkleTree[H] = {
        @tailrec
        def inner(seq: Seq[MerkleTree[H]]): MerkleTree[H] = {
            val newSeq: Seq[MerkleTree[H]] = seq.grouped(2).map(x => if x.length == 1 then x.head else MerkleTreeNode(x.head, x.last)).toSeq
            if newSeq.length > 1 then inner(newSeq) else newSeq.head
        }

        inner(seq.map(MerkleTreeLeaf.apply))
    }

    def mineMerkleTreeBlock(elements: Seq[String], priorBlockHash: Bytes): (Bytes, Bytes) = {
        val nonceBytes = 5 // The number of bytes to be used for the nonce.

        // Method to check if bytes begins with 20 bits.
        def checkBlockHash(bytes: Bytes): Boolean = bytes(0) == 0 && bytes(1) == 0 && (bytes(2) & 0xF0) == 0

        def rejectNonceWithHash(nwh: (Bytes, Bytes)): Boolean = !checkBlockHash(nwh._2)

        def nonceAndHash(nonce: Bytes, tree: MerkleTree[CryptoHash[SHA256]]): (Bytes, Bytes) = nonce -> tree.getHash.unsafeRunSync()

        val block = priorBlockHash +: (elements map (_.getBytes))
        val nonces = LazyList.continually(Random.nextBytes(nonceBytes))
        val candidates = nonces map (nonce => nonce -> MerkleTree(block :+ nonce))
        (candidates map nonceAndHash dropWhile rejectNonceWithHash take 1).to(List).head
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

    val tree: MerkleTree[CryptoHash[SHA256]] = MerkleTree(strings.map(_.getBytes()))
    tree.getHash.unsafeRunSync().bytes.foreach(x => print("%02X".format(x)))
    println()


    // block chain assignment
    val transactions = Seq(
        "Harry pays Robin 1.000",
        "Maharshi pays Harry 1.000",
        "Pranshu pays Maharshi 1.000",
        "Robin pays Pranshu 1.000"
    )

    val start = System.nanoTime()
    Random.setSeed(22)
    val (nonce, hash) = MerkleTree.mineMerkleTreeBlock(transactions, "00000dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824".getBytes())
    val end = System.nanoTime()
    val elapsed = end - start
    println("duration: " + elapsed + "ns")

    // validate
    println(util.Hex.bytesToHexString(nonce))
//    val tree2: MerkleTree[CryptoHash[SHA256]] = MerkleTree(transactions.head :+ "C188EB411C")
//    tree2.getHash.unsafeRunSync().bytes.foreach(x => print("%02X".format(x)))
//    println()
}