package crypto

import scala.collection.mutable
import util.Twisterator

type CompressionFunction = (Block, Block) => Block

/**
 * Trait to define the behavior of a class which can hash a BlockMessage into a Block.
 */
trait Hash {
    /**
     * A method to hash the given BlockMessage.
     *
     * @param message a BlockMessage.
     * @return a Block.
     */
    def hash(message: BlockMessage): Block
}

case class Block(bytes: Array[Byte]) {
    def length: Int = bytes.length

    override def toString: String =
        val sb = new mutable.StringBuilder()
        bytes.foreach(byte => sb.append("%02x".format(byte)))
        sb.toString
}

object Block {
    /**
     * Define the default block length (256 bits).
     *
     * Methods such as the apply method of this object (and others) can override the required length simply by passing in an explicit value for length.
     */
    implicit val requiredLength: Int = 16

    // NOTE: for now, countBytes is fixed.
    val countBytes = 4 // XXX Maximum number of count bytes: sufficient for a message of length of over 2 billion bytes.

    // NOTE: for now, padding is fixed.
    val padding: Byte = 0.toByte

    /**
     * Method to construct a Block from an Array[Byte] which checking on its length.
     * The length of the array is expected to match the (implicit) length (second parameter set).
     *
     * @param bytes  the array of bytes to make up the new Block.
     * @param nBytes (implicit) the expected length of the sequence (bytes).
     * @return a new Block.
     */
    def create(bytes: Array[Byte])(implicit nBytes: Int): Block = {
        require(bytes.length == nBytes, s"Block.apply: required length: $nBytes, actual length: ${bytes.length}")
        Block(bytes)
    }

    /**
     * Method to construct a Block from a Seq[Byte].
     * The length of the sequence is expected to match the (implicit) length (second parameter set).
     *
     * @param bytes  the sequence of bytes to make up the new Block.
     * @param nBytes (implicit) the expected length of the sequence (bytes).
     * @return a new Block.
     */
    def create(bytes: Seq[Byte])(implicit nBytes: Int): Block = create(bytes.toArray)

    /**
     * Method which is applied ONLY to the last block of a message.
     *
     * @param count  the size of the original message: which is to be encoded into the resulting (last) Block.
     * @param bytes  the sequence of bytes to make up the first part of the new Block. The remainder will be made up from padding and count.
     * @param nBytes (implicit) the expected length of the sequence (bytes).
     * @return a Block based with the final bytes of padding overwritten by the count.
     */
    def pad(count: Int)(bytes: Array[Byte])(implicit nBytes: Int): Block = {
        val countArray: Array[Byte] = BigInt(count).toByteArray
        val n = math.min(countBytes, countArray.length)
        assert(bytes.length + n <= nBytes, s"pad: too many bytes: #bytes=${bytes.length}, n=$n, nBytes=$nBytes")
        val result = new Array[Byte](nBytes)
        System.arraycopy(bytes, 0, result, 0, bytes.length)
        for (x <- bytes.length until nBytes) result(x) = padding
        System.arraycopy(countArray, countArray.length - n, result, nBytes - n, n)
        Block(result)
    }
}

/**
 * Case class which implements Hash.
 *
 * @param cf a CompressionFunction.
 * @param iv the initialization vector.
 */
case class BlockHash(cf: CompressionFunction, iv: Block) extends Hash :
    /**
     * A method to hash the given BlockMessage.
     *
     * @param message a BlockMessage.
     * @return a Block returned by invoking foldLeft(iv)(cf) on the given message.
     */
    def hash(message: BlockMessage): Block = message.foldLeft(iv)(cf)

/**
 * A case class to represent a sequence of Blocks.
 *
 * @param blocks the Blocks making up this BlockMessage.
 */
case class BlockMessage(blocks: Seq[Block]) {
    /**
     * Method to apply foldLeft to the blocks of this BlockMessage.
     *
     * @param z0 the Z value returned for an empty sequence of Blocks.
     * @param f  the applicative function of type (Z, Block) => Z.
     * @tparam Z the type to be returned (and therefore the type of z0).
     * @return a value of Z.
     */
    def foldLeft[Z](z0: => Z)(f: (Z, Block) => Z): Z = blocks.foldLeft(z0)(f)

    /**
     * Form a String for this BlockMessage by successively appending a String corresponding to each block,
     * using the render method.
     *
     * @return
     */
    override def toString: String =
        val sb = new mutable.StringBuilder()
        blocks.foreach(b => sb.append(b.toString))
        sb.toString
}

object BlockMessage {

    import Block.requiredLength

    /**
     * Method to construct a BlockMessage from a Block (a byte array of any length).
     *
     * @param message an array of Byte of any length.
     * @param nBytes  (implicit) the number of bytes to be placed in each of the resulting blocks.
     * @return a BlockMessage where each Block is of length nBytes.
     */
    def apply(message: Array[Byte])(implicit nBytes: Int): BlockMessage = BlockMessage(prepareBlocks(message))

    /**
     * Method to construct a BlockMessage from an iterator of Blocks.
     *
     * @param blocks an iterator of Blocks.
     * @return a BlockMessage based on a Seq[Block].
     */
    def apply(blocks: Iterator[Block]): BlockMessage = BlockMessage(blocks.toSeq)

    /**
     * Method to construct a BlockMessage from a String.
     *
     * @param message        a String of characters.
     * @param requiredLength (implicit) the number of bytes to be placed in each of the resulting blocks.
     * @return a BlockMessage.
     */
    def apply(message: String)(implicit requiredLength: Int): BlockMessage = apply(message.getBytes())

    /**
     * Implicit class to enable the "xor" operator.
     *
     * @param block a Block.
     */
    implicit class Xor(block: Block) {
        /**
         * Method to XOR block with other.
         * NOTE: the sizes of the blocks should be the same but, in any case, the length of the output will be that of the shortest block.
         *
         * @param other a Block.
         * @return
         */
        infix def xor(other: Block): Block = Block(for ((b1, b2) <- block.bytes zip other.bytes) yield xor(b1, b2))

        private def xor(b1: Byte, b2: Byte) = (b1.toInt ^ b2.toInt).toByte
    }

    /**
     * Method to prepare an iterator of Blocks which can form a BlockMessage from a message.
     *
     * @param message an array of Byte of any length.
     * @param nBytes  (implicit) the number of bytes to be placed in each of the resulting blocks.
     * @return an Iterator[Block].
     */
    private def prepareBlocks(message: Array[Byte])(implicit nBytes: Int): Iterator[Block] = {
        val count = message.length
        val bytes =
            if (count % nBytes + Block.countBytes > nBytes) {
                val extendedArray: Array[Byte] = new Array[Byte](count + Block.countBytes)
                System.arraycopy(message, 0, extendedArray, 0, count)
                extendedArray
            }
            else message
        val byteArrays = for (b <- bytes.grouped(nBytes)) yield b
        val create: Array[Byte] => Block = Block.create
        Twisterator(create, Block.pad(count))(byteArrays)
    }
}