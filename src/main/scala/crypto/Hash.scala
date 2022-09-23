package crypto

type Block = Array[Byte]

type CompressionFunction = (Block, Block) => Block

trait Hash {
    def hash(message: BlockMessage): Block
}

case class BlockHash(bits: Int, cf: CompressionFunction, iv: Block) extends Hash :
    def hash(message: BlockMessage): Block = message.blocks.foldLeft(iv)((r, b) => cf(r, b))

case class BlockMessage(blocks: Iterator[Block])

object BlockMessage {
    def apply(bytes: Int, block: Block): BlockMessage = {
        val rawBlocks: Iterator[Array[Byte]] = block.grouped(bytes)
        BlockMessage(for (b <- rawBlocks) yield pad(bytes, b))
    }

    def apply(bytes: Int, message: String): BlockMessage = apply(bytes, message.getBytes())

    val padding: Byte = 'Q'.toByte

    def pad(bytes: Int, block: Block): Block = if block.length == bytes then block else {
        val result = new Array[Byte](bytes)
        System.arraycopy(block, 0, result, 0, block.length)
        for (x <- block.length until bytes) result(x) = padding
        result
    }

    implicit class Xor(block: Block) {
        def xor(other: Block): Block = {
            val zipped: Array[(Byte, Byte)] = block zip other
            val result: Block = for ((b1, b2) <- zipped) yield (b1.toInt ^ b2.toInt).toByte
            result
        }
    }
}