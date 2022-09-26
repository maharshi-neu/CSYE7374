package crypto

import scala.collection.mutable

type Block = Array[Byte]

type CompressionFunction = (Block, Block) => Block

trait Hash {
    def hash(message: BlockMessage): Block
}

case class BlockHash(bits: Int, cf: CompressionFunction, iv: Block) extends Hash :
    def hash(message: BlockMessage): Block = message.blocks.foldLeft(iv)((r, b) => cf(r, b))

case class BlockMessage(blocks: Seq[Block]) {
    override def toString: String = {
        val sb = new mutable.StringBuilder()
        blocks.foreach(b => sb.append(BlockMessage.render(b)))
        sb.toString
    }
}

object BlockMessage {
    def apply(bytes: Int, block: Block): BlockMessage = {
        val rawBlocks: Iterator[Array[Byte]] = block.grouped(bytes)
        BlockMessage(for (b <- rawBlocks) yield pad(bytes, b))
    }

    def apply(blocks: Iterator[Block]): BlockMessage = BlockMessage(blocks.toSeq)

    def apply(bytes: Int, message: String): BlockMessage = apply(bytes, message.getBytes())

    val padding: Byte = 0.toByte

    def pad(bytes: Int, block: Block): Block = if block.length == bytes then block else {
        val result = new Array[Byte](bytes)
        System.arraycopy(block, 0, result, 0, block.length)
        for (x <- block.length until bytes) result(x) = padding
        result
    }

    implicit class Xor(block: Block) {
        /**
         * Method to XOR block with other.
         * NOTE: the sizes of the blocks should be the same but, in any case, the length of the output will be that of the shortest block.
         *
         * @param other a Block.
         * @return
         */
        infix def xor(other: Block): Block = for ((b1, b2) <- block zip other) yield xor(b1, b2)

        private def xor(b1: Byte, b2: Byte) = (b1.toInt ^ b2.toInt).toByte
    }

    def render(b: Block): String = {
        val sb = new mutable.StringBuilder()
        b.foreach(byte => sb.append("%02x".format(byte)))
        sb.toString
    }
}